# -*- coding: utf-8 -*- #
# Copyright 2018 Google Inc. All Rights Reserved.
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
"""Command to remove an IAM policy binding from an disk resource."""

from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals

from googlecloudsdk.api_lib.compute import base_classes
from googlecloudsdk.calliope import base
from googlecloudsdk.command_lib.compute.disks import flags as disks_flags
from googlecloudsdk.command_lib.iam import iam_util


@base.ReleaseTracks(base.ReleaseTrack.ALPHA, base.ReleaseTrack.BETA)
class RemoveIamPolicyBinding(base.Command):
  """Remove an IAM policy binding from a Google Compute Engine disk.

  *{command}* removes an IAM policy binding from a Google Compute Engine
  disk's access policy.
  """
  detailed_help = iam_util.GetDetailedHelpForRemoveIamPolicyBinding(
      'disk', 'my-disk', role='roles/compute.securityAdmin')

  @staticmethod
  def Args(parser):
    RemoveIamPolicyBinding.disk_arg = disks_flags.MakeDiskArg(plural=False)
    RemoveIamPolicyBinding.disk_arg.AddArgument(
        parser, operation_type='remove the IAM policy binding from')
    iam_util.AddArgsForRemoveIamPolicyBinding(parser)

  def Run(self, args):
    holder = base_classes.ComputeApiHolder(self.ReleaseTrack())
    client = holder.client
    disk_ref = RemoveIamPolicyBinding.disk_arg.ResolveAsResource(
        args, holder.resources)
    get_request = client.messages.ComputeDisksGetIamPolicyRequest(
        resource=disk_ref.disk, zone=disk_ref.zone, project=disk_ref.project)
    policy = client.apitools_client.disks.GetIamPolicy(get_request)
    iam_util.RemoveBindingFromIamPolicy(policy, args.member, args.role)
    # TODO(b/78371568): Construct the ZoneSetPolicyRequest directly
    # out of the parsed policy.
    set_request = client.messages.ComputeDisksSetIamPolicyRequest(
        resource=disk_ref.disk,
        zone=disk_ref.zone,
        zoneSetPolicyRequest=client.messages.ZoneSetPolicyRequest(
            bindings=policy.bindings,
            etag=policy.etag),
        project=disk_ref.project)
    return client.apitools_client.disks.SetIamPolicy(set_request)
