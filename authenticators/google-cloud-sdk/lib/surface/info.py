# -*- coding: utf-8 -*- #
# Copyright 2013 Google Inc. All Rights Reserved.
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

"""A command that prints out information about your gcloud environment."""

from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals

from googlecloudsdk.calliope import base
from googlecloudsdk.command_lib import info_holder
from googlecloudsdk.core import log
from googlecloudsdk.core.diagnostics import network_diagnostics
from googlecloudsdk.core.diagnostics import property_diagnostics


def _RunDiagnostics():
  network_diagnostics.NetworkDiagnostic().RunChecks()
  property_diagnostics.PropertyDiagnostic().RunChecks()


@base.ReleaseTracks(base.ReleaseTrack.GA)
class Info(base.Command):
  """Display information about the current gcloud environment.

     {command} displays information about the current gcloud environment.

     - {command} will print information about the current active configuration,
       including the Google Cloud Platform account, project and directory paths
       for logs.

     - {command} --run-diagnostics will run a checks on network connectivity.

     - {command} --show-log prints the contents of the most recent log file.
  """

  @staticmethod
  def Args(parser):
    parser.add_argument(
        '--show-log',
        action='store_true',
        help='Print the contents of the last log file.')
    parser.add_argument(
        '--run-diagnostics',
        action='store_true',
        help='Run diagnostics on your installation of the Cloud SDK.')
    parser.add_argument(
        '--anonymize',
        action='store_true',
        help='Minimize any personal identifiable information. '
             'Use it when sharing output with others.')

  def Run(self, args):
    if args.run_diagnostics:
      _RunDiagnostics()
      return
    holder = info_holder.InfoHolder(
        anonymizer=info_holder.Anonymizer()
        if args.anonymize else info_holder.NoopAnonymizer())
    return holder

  def Display(self, args, info):
    if not args.run_diagnostics:
      log.Print(info)

    if args.show_log and info.logs.last_log:
      log.Print('\nContents of log file: [{0}]\n'
                '==========================================================\n'
                '{1}\n\n'
                .format(info.logs.last_log, info.logs.LastLogContents()))

