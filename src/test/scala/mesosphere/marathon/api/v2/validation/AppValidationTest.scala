package mesosphere.marathon
package api.v2.validation

import com.wix.accord.scalatest.ResultMatchers
import mesosphere.{ UnitTest, ValidationTestLike }
import mesosphere.marathon.raml._
import com.wix.accord._
import mesosphere.UnitTest

class AppValidationTest extends UnitTest with ResultMatchers with ValidationTestLike {

  import Normalization._

  "canonical app validation" when {

    val basicValidator = AppValidation.validateCanonicalAppAPI(Set.empty)

    "multiple container networks are specified for an app" should {

      val app = App(id = "/foo", cmd = Some("bar"), networks = 1.to(2).map(i => Network(mode = NetworkMode.Container, name = Some(i.toString))))

      // we don't allow this yet because Marathon doesn't yet support per-network port-mapping (and it's not meaningful
      // for a single host port to map to the same container port on multiple network interfaces).
      "require networkName for containerPort to hostPort mapping" in {
        val ct = Container(`type` = EngineType.Mesos, portMappings = Some(Seq(ContainerPortMapping(hostPort = Option(0)))))
        val badApp = app.copy(container = Some(ct))
        basicValidator(badApp).normalize should failWith(
          "/container/portMappings(0)/networkName" -> "must not be empty",
          "/container/portMappings(0)/hostPort" -> "must be empty")
      }

      "allow portMappings that don't declare hostPort nor networkName" in {
        val ct = Container(`type` = EngineType.Mesos, portMappings = Some(Seq(ContainerPortMapping())))
        val badApp = app.copy(container = Some(ct))
        basicValidator(badApp) should be(aSuccess)
      }

      "allow portMappings that both declare a hostPort and a networkName" in {
        val ct = Container(`type` = EngineType.Mesos, portMappings = Some(Seq(
          ContainerPortMapping(
            hostPort = Option(0),
            networkName = Some("1")))))
        val badApp = app.copy(container = Some(ct))
        basicValidator(badApp) should be(aSuccess)
      }
    }
  }

  "portMapping validation" when {
    "single container network" should {
      val validPortMapping = ContainerPortMapping(
        hostPort = Some(80),
        containerPort = 80,
        protocol = NetworkProtocol.Tcp,
        name = Some("http-port"),
        labels = Map("foo" -> "bar"))

      val validNetworks = List(Network(Some("container-network"), NetworkMode.Container))
      implicit val portMappingValidator =
        AppValidation.portMappingsValidator(validNetworks)

      "consider a valid portMapping as valid" in {
        validate(Seq(validPortMapping)) should be(Success)
      }

      "consider a portMapping with no name as valid" in {
        validate(Seq(validPortMapping.copy(name = None))) should be(Success)
      }

      "maybe consider a portMapping without hostport as valid" in {
        validate(Seq(validPortMapping.copy(hostPort = None))) should be(Success)
      }

      "consider portMapping with zero hostport as valid" in {
        validate(Seq(validPortMapping.copy(hostPort = Some(0)))) should be(Success)
      }

      "consider portMapping with a matching network name as valid" in {
        validate(Seq(validPortMapping.copy(networkName = Some("container-network")))) should be(Success)
      }

      "consider portMapping with a non-matching network name as invalid" in {
        val result = validate(Seq(validPortMapping.copy(networkName = Some("invalid-network-name"))))
        result.isFailure shouldBe true
      }

      "consider portMapping without networkName nor hostPort as valid" in {
        validate(Seq(validPortMapping.copy(networkName = None, hostPort = None))) should be(Success)
      }
    }
  }
}
