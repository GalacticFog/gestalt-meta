# -*- coding: utf-8 -*- #
# Copyright 2017 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""Useful commands for interacting with the Cloud Filestore API."""

from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals

from apitools.base.py import list_pager

from googlecloudsdk.api_lib.compute import utils
from googlecloudsdk.api_lib.util import apis
from googlecloudsdk.api_lib.util import waiter
from googlecloudsdk.core import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import resources


FILESTORE_API_NAME = 'file'
FILESTORE_ALPHA_API_VERSION = 'v1alpha1'
FILESTORE_API_VERSION = 'v1beta1'
OPERATIONS_COLLECTION = 'file.projects.locations.operations'


def GetClient(version=FILESTORE_API_VERSION):
  """Import and return the appropriate Cloud Filestore client.

  Args:
    version: str, the version of the API desired.

  Returns:
    Cloud Filestore client for the appropriate release track.
  """
  return apis.GetClientInstance(FILESTORE_API_NAME, version)


def GetMessages(version=FILESTORE_API_VERSION):
  """Import and return the appropriate Filestore messages module."""
  return apis.GetMessagesModule(FILESTORE_API_NAME, version)


class Error(exceptions.Error):
  """Base class for exceptions in this module."""


class InvalidCapacityError(Error):
  """Raised when an invalid capacity value is provided."""


class InvalidNameError(Error):
  """Raised when an invalid file share name value is provided."""


class FilestoreClient(object):
  """Wrapper for working with the file API."""

  def __init__(self, version=FILESTORE_API_VERSION):
    if version == FILESTORE_ALPHA_API_VERSION:
      self._adapter = AlphaFilestoreAdapter()
    elif version == FILESTORE_API_VERSION:
      self._adapter = BetaFilestoreAdapter()
    else:
      raise ValueError('[{}] is not a valid API version.'.format(version))

  @property
  def client(self):
    return self._adapter.client

  @property
  def messages(self):
    return self._adapter.messages

  def ListInstances(self, location_ref, limit=None):  # pylint: disable=redefined-builtin
    """Make API calls to List active Cloud Filestore instances.

    Args:
      location_ref: The parsed location of the listed Filestore instances.
      limit: The number of Cloud Filestore instances to limit the results to.
        This limit is passed to the server and the server does the limiting.

    Returns:
      Generator that yields the Cloud Filestore instances.
    """
    request = self.messages.FileProjectsLocationsInstancesListRequest(
        parent=location_ref.RelativeName())
    # Check for unreachable locations.
    response = self.client.projects_locations_instances.List(request)
    for location in response.unreachable:
      log.warning('Location {} may be unreachable.'.format(location))
    return list_pager.YieldFromList(
        self.client.projects_locations_instances,
        request,
        field='instances',
        limit=limit,
        batch_size_attribute='pageSize')

  def GetInstance(self, instance_ref):
    """Get Cloud Filestore instance information."""
    request = self.messages.FileProjectsLocationsInstancesGetRequest(
        name=instance_ref.RelativeName())
    return self.client.projects_locations_instances.Get(request)

  def DeleteInstance(self, instance_ref, async_):
    """Delete an existing Cloud Filestore instance."""
    request = self.messages.FileProjectsLocationsInstancesDeleteRequest(
        name=instance_ref.RelativeName())
    delete_op = self.client.projects_locations_instances.Delete(request)
    if async_:
      return delete_op
    operation_ref = resources.REGISTRY.ParseRelativeName(
        delete_op.name, collection=OPERATIONS_COLLECTION)
    return self.WaitForOperation(operation_ref)

  def GetOperation(self, operation_ref):
    """Gets description of a long-running operation.

    Args:
      operation_ref: the operation reference.

    Returns:
      messages.GoogleLongrunningOperation, the operation.
    """
    request = self.messages.FileProjectsLocationsOperationsGetRequest(
        name=operation_ref.RelativeName())
    return self.client.projects_locations_operations.Get(request)

  def WaitForOperation(self, operation_ref):
    """Waits on the long-running operation until the done field is True.

    Args:
      operation_ref: the operation reference.

    Raises:
      waiter.OperationError: if the operation contains an error.

    Returns:
      the 'response' field of the Operation.
    """
    return waiter.WaitFor(
        waiter.CloudOperationPollerNoResources(
            self.client.projects_locations_operations), operation_ref,
        'Waiting for [{0}] to finish'.format(operation_ref.Name()))

  def CreateInstance(self, instance_ref, async_, config):
    """Create a Cloud Filestore instance."""
    request = self.messages.FileProjectsLocationsInstancesCreateRequest(
        parent=instance_ref.Parent().RelativeName(),
        instanceId=instance_ref.Name(),
        instance=config)
    create_op = self.client.projects_locations_instances.Create(request)
    if async_:
      return create_op
    operation_ref = resources.REGISTRY.ParseRelativeName(
        create_op.name, collection=OPERATIONS_COLLECTION)
    return self.WaitForOperation(operation_ref)

  def GetLocation(self, location_ref):
    request = self.messages.FileProjectsLocationsGetRequest(
        name=location_ref.RelativeName())
    return self.client.projects_locations.Get(request)

  def ListLocations(self, project_ref, limit=None):
    request = self.messages.FileProjectsLocationsListRequest(
        name=project_ref.RelativeName())
    return list_pager.YieldFromList(
        self.client.projects_locations,
        request,
        field='locations',
        limit=limit,
        batch_size_attribute='pageSize')

  def ListOperations(self, operation_ref, limit=None):  # pylint: disable=redefined-builtin
    """Make API calls to List active Cloud Filestore operations.

    Args:
      operation_ref: The parsed location of the listed Filestore instances.
      limit: The number of Cloud Filestore instances to limit the results to.
        This limit is passed to the server and the server does the limiting.

    Returns:
      Generator that yields the Cloud Filestore instances.
    """
    request = self.messages.FileProjectsLocationsOperationsListRequest(
        name=operation_ref.RelativeName())
    return list_pager.YieldFromList(
        self.client.projects_locations_operations,
        request,
        field='operations',
        limit=limit,
        batch_size_attribute='pageSize')

  def _ValidateFileShare(self, instance_tier, capacity_gb):
    """Validates the value of the file share capacity."""
    gb_in_one_tb = 1 << 10
    minimum_values = {
        self.messages.Instance.TierValueValuesEnum.STANDARD: gb_in_one_tb,
        self.messages.Instance.TierValueValuesEnum.PREMIUM: 2.5 * gb_in_one_tb}
    minimum = minimum_values.get(instance_tier, 0)
    if capacity_gb < minimum:
      raise InvalidCapacityError(
          'File share capacity must be greater than or equal to {}TB for a {} '
          'instance.'.format(minimum / gb_in_one_tb, instance_tier))

  def ValidateFileShares(self, instance):
    """Validate the file share configs on the instance."""
    for volume in self._adapter.FileSharesFromInstance(instance):
      if volume.capacityGb:
        self._ValidateFileShare(instance.tier, volume.capacityGb)

  def ParseFilestoreConfig(self, tier=None, description=None, file_share=None,
                           network=None, labels=None):
    """Parses the command line arguments for Create into a config.

    Args:
      tier: the tier.
      description: the description of the instance.
      file_share: the config for the file share.
      network: The network for the instance.
      labels: The parsed labels value.

    Returns:
      the configuration that will be used as the request body for creating a
      Cloud Filestore instance.
    """
    instance = self.messages.Instance()

    instance.tier = tier
    instance.labels = labels

    if description:
      instance.description = description

    self._adapter.ParseFileShareIntoInstance(instance, file_share)

    if network:
      instance.networks = []
      network_config = self.messages.NetworkConfig()
      network_config.network = network.get('name')
      if 'reserved-ip-range' in network:
        network_config.reservedIpRange = network['reserved-ip-range']
      instance.networks.append(network_config)
    return instance

  def ParseUpdatedInstanceConfig(self, instance_config, description=None,
                                 labels=None, file_share=None):
    """Parses updates into an instance config.

    Args:
      instance_config: The Instance message to update.
      description: str, a new description, if any.
      labels: LabelsValue message, the new labels value, if any.
      file_share: dict representing a new file share config, if any.

    Raises:
      InvalidCapacityError, if an invalid capacity value is provided.
      InvalidNameError, if an invalid file share name is provided.

    Returns:
      The instance message.
    """
    instance = self._adapter.ParseUpdatedInstanceConfig(
        instance_config, description=description, labels=labels,
        file_share=file_share)
    return instance

  def UpdateInstance(self, instance_ref, instance_config, update_mask, async_):
    """Updates an instance.

    Args:
      instance_ref: the reference to the instance.
      instance_config: Instance message, the updated instance.
      update_mask: str, a comma-separated list of updated fields.
      async_: bool, if False, wait for the operation to complete.

    Returns:
      an Operation or Instance message.
    """
    update_op = self._adapter.UpdateInstance(
        instance_ref, instance_config, update_mask)
    if async_:
      return update_op
    operation_ref = resources.REGISTRY.ParseRelativeName(
        update_op.name, collection=OPERATIONS_COLLECTION)
    return self.WaitForOperation(operation_ref)


class AlphaFilestoreAdapter(object):
  """Adapter for the alpha filestore API."""

  def __init__(self):
    self.client = GetClient(version=FILESTORE_ALPHA_API_VERSION)
    self.messages = GetMessages(version=FILESTORE_ALPHA_API_VERSION)

  def ParseFileShareIntoInstance(self, instance, file_share):
    """Parse specified file share configs into an instance message."""
    if instance.volumes is None:
      instance.volumes = []
    if file_share:
      file_share_config = self.messages.VolumeConfig(
          name=file_share.get('name'),
          capacityGb=utils.BytesToGb(file_share.get('capacity')))
      instance.volumes.append(file_share_config)

  def FileSharesFromInstance(self, instance):
    """Get file share configs from instance message."""
    return instance.volumes

  def UpdateInstance(self, instance_ref, instance_config, update_mask):
    # Not supported by alpha API.
    raise NotImplementedError

  def ParseUpdatedInstanceConfig(self, instance_config, description=None,
                                 labels=None, file_shares=None):
    # Not supported by alpha API.
    raise NotImplementedError


class BetaFilestoreAdapter(object):
  """Adapter for the beta filestore API."""

  def __init__(self):
    self.client = GetClient(version=FILESTORE_API_VERSION)
    self.messages = GetMessages(version=FILESTORE_API_VERSION)

  def ParseFileShareIntoInstance(self, instance, file_share):
    """Parse specified file share configs into an instance message."""
    if instance.fileShares is None:
      instance.fileShares = []
    if file_share:
      file_share_config = self.messages.FileShareConfig(
          name=file_share.get('name'),
          capacityGb=utils.BytesToGb(file_share.get('capacity')))
      instance.fileShares.append(file_share_config)

  def FileSharesFromInstance(self, instance):
    """Get fileshare configs from instance message."""
    return instance.fileShares

  def UpdateInstance(self, instance_ref, instance_config, update_mask):
    """Send a Patch request for the Cloud Filestore instance."""
    update_request = self.messages.FileProjectsLocationsInstancesPatchRequest(
        instance=instance_config,
        name=instance_ref.RelativeName(),
        updateMask=update_mask)
    update_op = self.client.projects_locations_instances.Patch(update_request)
    return update_op

  def ParseUpdatedInstanceConfig(self, instance_config, description=None,
                                 labels=None, file_share=None):
    """Parse update information into an updated Instance message."""
    if description:
      instance_config.description = description
    if labels:
      instance_config.labels = labels
    if file_share:
      self.ValidateFileShareForUpdate(instance_config, file_share)
      self.ParseFileShareIntoInstance(instance_config, file_share)
    return instance_config

  def ValidateFileShareForUpdate(self, instance_config, file_share):
    """Validate the updated file share configuration.

    The new config must have the same name as the existing config and a larger
    size than the existing capacity.

    Args:
      instance_config: Instance message for existing instance.
      file_share: dict with keys 'name' and 'capacity'.

    Raises:
      InvalidNameError: If the names don't match.
      InvalidCapacityError: If the capacity is not larger.
      ValueError: If the instance doesn't have an existing file share.
    """
    existing = self.FileSharesFromInstance(instance_config)
    if not existing:
      # This should never happen because all instances have one file share.
      raise ValueError('Existing instance does not have file shares configured')
    existing_file_share = existing[0]
    if existing_file_share.name != file_share.get('name'):
      raise InvalidNameError(
          'Must resize an existing file share. Existing file share is named '
          '[{}]. Requested update had name [{}].'.format(
              existing_file_share.name, file_share.get('name')))
    new_capacity = utils.BytesToGb(file_share.get('capacity'))
    if not existing_file_share.capacityGb < new_capacity:
      raise InvalidCapacityError(
          'Must resize the file share to a larger capacity. Existing capacity: '
          '[{}]. New capacity requested: [{}].'.format(
              existing_file_share.capacityGb, new_capacity))
