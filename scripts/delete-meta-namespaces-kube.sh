#!/bin/bash

###
### USE WITH CAUTION!!!
### This script uses your default kube config file (~/.kube/config) and deletes all
### namespaces with names that look like v4 UUIDs
###

kubectl get namespaces --no-headers \
	| awk '/^[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89aAbB][a-f0-9]{3}-[a-f0-9]{12}/{ print $1 }' \
	| xargs kubectl delete namespaces
	
	