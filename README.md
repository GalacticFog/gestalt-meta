gestalt-meta
---------------

Persistence service for Gestalt Framework Resources. 


  - Org
  - User
  - Group
  - Workspace
  - Environment
  - Container
  - Lambda
  - ApiEndpoint
  - ApiGateway
  - Policy
  - Rule
  - Entitlement
  - ApiGatewayProvider
  - MarathonProvider


####  Required Environment Variables

    ### gestalt-security
    export GESTALT_SECURITY_PROTOCOL="https"
    export GESTALT_SECURITY_HOSTNAME="https://security.yourcompany.com"
    export GESTALT_SECURITY_PORT=9455
    export GESTALT_SECURITY_KEY="{your-key}"
    export GESTALT_SECURITY_SECRET="{your-secret}"

    ### gestalt-lambda and gestalt-apigateway
    export GESTALT_LAMBDA="https://lambda.yourcompany.com"
    export GESTALT_APIGATEWAY="https://gateway.yourcompany.com"

    ### RabbitMQ
    export RABBIT_HTTP_PROTOCOL="https"
    export RABBIT_HOST="rabbit.example.com"
    
    # The broker port (amqp)
    export RABBIT_PORT=5672 
    
    # REST API Port for health-check
    export RABBIT_HTTP_PORT=443 
    
    export RABBIT_ROUTE="policy"
    export RABBIT_EXCHANGE="policy-exchange"

    
## Audit Logging
Configured with the following Environment Variables:

| Variable | Default | Description |
|----------|----------|-------------|
| META_AUDIT_ENABLED        | false     | Controls audit-logging overall           |
| META_AUDIT_LOGGER_NAME    | *required*| Name for the logger as it appears in log file. NOTE: Default to 'AUDIT'|
| META_AUDIT_LOG_FILE       | *required*| Name/path to log file
| META_AUDIT_LOG_EXCLUSIVE  | true      | Log exclusively to named log file when `true`|
| META_AUDIT_LOG_ROLLOVER   |           | Set value for rolling logs: (month, day, hour, minute, none)|
| META_AUDIT_EXIT_FAILURE   | false     | [in progress]|

### Check Status of Audit Service
    GET /root/check?feature=audit
This will return basic information about the current status and setting for auditing: 
```{
    "config_state": "OK",
    "enabled": "true",
    "settings": {
        "META_AUDIT_LOG_FILE": "meta-audit.log",
        "META_AUDIT_ENABLED": "true",
        "META_AUDIT_LOG_EXCLUSIVE": "true",
        "META_AUDIT_LOG_LEVEL": "info",
        "META_AUDIT_LOG_ROLLOVER": "day",
        "META_AUDIT_LOGGER_NAME": "AUDIT"
    }
}
```
