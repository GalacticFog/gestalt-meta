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
"""Exceptions for concept args."""

from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals

from googlecloudsdk.core import exceptions


class Error(exceptions.Error):
  """Base class for errors in this module."""


class ParseError(Error):
  """Error when parsing a concept."""

  def __init__(self, concept_name, message):
    super(ParseError, self).__init__(
        'Failed to parse [{}]. {}'.format(concept_name, message))


class MissingRequiredArgumentException(Error):
  """Error when a required concept can't be found."""

  def __init__(self, concept_name, message):
    super(MissingRequiredArgumentException, self).__init__(
        'No value was provided for [{}]: {}'.format(concept_name, message))
