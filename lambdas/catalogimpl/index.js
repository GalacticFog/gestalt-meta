
exports.entryPoint = function(event, context, callback) {

const catalogList =`
<!DOCTYPE html>
<html>
<head>
<title>Service Catalog</title>
<link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-rc.2/css/materialize.min.css">
<script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-rc.2/js/materialize.min.js"></script>
</head>
<body>
  <nav class="light-blue lighten-1" role="navigation">
    <div class="nav-wrapper container"><a id="logo-container" href="#" class="brand-logo">Logo</a>
      <ul class="right hide-on-med-and-down">
        <li><a href="#">Navbar Link</a></li>
      </ul>

      <ul id="nav-mobile" class="sidenav">
        <li><a href="#">Navbar Link</a></li>
      </ul>
      <a href="#" data-target="nav-mobile" class="sidenav-trigger"><i class="material-icons">menu</i></a>
    </div>
  </nav>

  <div class="section no-pad-bot" id="index-banner">
    <div class="container">
      <br><br>
      <h1 class="header center orange-text">Gestalt Service Catalog</h1>
      <div class="row center">
        <h5 class="header col s12 light">Explore, launch, and manage solutions in just a few clicks</h5>
      </div>
      <div class="row center">
        <a href="https://test.galacticfog.com" id="download-button" class="btn-large waves-effect waves-light orange">Get Started</a>
      </div>
      <br><br>

    </div>
  </div>      

  <div class="container">
    <div class="section">

      <!--   Icon Section   -->
      <div class="row">
        <div class="col s12 m4">
          <div class="icon-block">
            <h2 class="center light-blue-text"><i class="material-icons">flash_on</i></h2>
            <h5 class="center">Speeds up development</h5>
            <p class="light">
                Service Catalog let's you quickly deploy software on the Gestalt Platform in just a few clicks</p>
          </div>
        </div>

        <div class="col s12 m4">
          <div class="icon-block">
            <h2 class="center light-blue-text"><i class="material-icons">group</i></h2>
            <h5 class="center">User Experience Focused</h5>
            <p class="light">
                Lomo keytar gentrify shoreditch salvia occupy iPhone food truck williamsburg hammock try-hard. Flexitarian stumptown food truck, sartorial tumblr scenester leggings bespoke pour-over. 
            </p>
          </div>
        </div>

        <div class="col s12 m4">
          <div class="icon-block">
            <h2 class="center light-blue-text"><i class="material-icons">settings</i></h2>
            <h5 class="center">Easy to work with</h5>
            <p class="light">
                Portland aesthetic tofu franzen brunch crucifix ugh cardigan 8-bit photo booth banjo. Beard selvage tote bag live-edge organic mustache quinoa 8-bit. Art party vice swag, vaporware bespoke meh lumbersexual lomo whatever af migas lyft gastropub 90's.
            </p>
          </div>
        </div>
      </div>

    </div>
    <br><br>
  </div>
  <footer class="page-footer orange">
      <div class="container">
      </div>
  </footer>        

  <script src="https://code.jquery.com/jquery-2.1.1.min.js"></script>  
</body>
</html>
`
  const contextData = JSON.parse(context)
  const eventData = JSON.parse(event)
  const eventName = eventData.action
  const fn = new Map()

  fn.set('catalog.list', function(e, c, n, b) { callback(null, catalogList) })

  const process = (eventName) => {
    console.log(`Received event '${eventName}'...finding handler...`)
    if (fn.has(eventName)) {
      console.log("Calling '${eventName}'")
      fn.get(eventName)(eventData, contextData, null, callback)
    } else {
      callback(new Error(`Invalid event-name. found : '${eventName}'`))
    }
  }

  process(eventName)
}