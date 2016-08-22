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

    
