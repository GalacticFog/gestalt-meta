package services.kubernetes

object KubernetesConstants {
  val META_CONTAINER_KEY = "meta/container"
  val META_SECRET_KEY = "meta/secret"
  val META_VOLUME_KEY = "meta/volume"
  val META_JOB_KEY = "meta/job"
  val META_ENVIRONMENT_KEY = "meta/environment"
  val META_WORKSPACE_KEY = "meta/workspace"
  val META_FQON_KEY = "meta/fqon"
  val META_PROVIDER_KEY = "meta/provider"

  // val CPU_REQ_TYPE = "cpu-requirement-type"
  // val MEM_REQ_TYPE = "memory-requirement-type"
  val CPU_REQ_TYPE = "cpu_requirement_type"
  val MEM_REQ_TYPE = "memory_requirement_type"

  val REQ_TYPE_LIMIT = "limit"
  val REQ_TYPE_REQUEST = "request"

  val DEFAULT_CPU_REQ = REQ_TYPE_REQUEST
  val DEFAULT_MEM_REQ = Seq(REQ_TYPE_LIMIT,REQ_TYPE_REQUEST).mkString(",")

  val HOST_VOLUME_WHITELIST = "host_volume_whitelist"
  val STORAGE_CLASSES = "storage_classes"
}