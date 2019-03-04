package services.kubernetes

import com.galacticfog.gestalt.util.Error
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.meta.providers.gwm.{ApiProperties,ApiEndpointProperties}
import services.kubernetes.istio.networking.v1alpha3.gateway.Gateway
import services.kubernetes.istio.networking.v1alpha3.virtual_service.VirtualService

trait IstioEntityBuilder {
  def mkGateway(providerProperties: KubernetesProviderProperties.Properties, apiProperties: ApiProperties): EitherError[Gateway] = {
    Left(Error.Default("Not implemented"))
  }

  def mkVirtualService(providerProperties: KubernetesProviderProperties.Properties, endpointProperties: ApiEndpointProperties): EitherError[VirtualService] = {
    Left(Error.Default("Not implemented"))
  }
}