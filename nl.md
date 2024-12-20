# Overview

Instructions on embedding the Curling I/O V2 Widgets onto the Newfoundland & Labrador Curling Association wordpress site.

You'll first need the ability to embed raw javascript code into your pages and sidebar.
If you don't have a plugin or theme that allows this yet, you could potentially install: https://en-ca.wordpress.org/plugins/simple-embed-code/


## Scoreboard Widget

The scoreboard screen lists all of your "active" competitions. Clicking into a competition will load it's schedule and results.
Currently this is https://curlingnl.ca/scoreboard/

You can use this same javascript code, but customizing the parameters to load a specific competition, or a list of competitions / leagues / products with registration buttons.

Place the following raw javascript code in to your scoreboard page, customizing the parameters as you see fit:

    <!-- REQUIRED. This script tag is required to load the widget. -->
    <!-- Ideally this goes in the head of your html document, but you can play around to see what works (depending on your CMS) -->
    <script src="https://pairshaped.github.io/curlingio-results/prod.min.js"></script>

    <!-- REQUIRED. This is the div the widget replaces. The ID of this div must match the getElemenetById below that's passed to the node. If you change the ID here, make sure you also change it down there. -->
    <div id="cio_results"></div>

    <!-- REQUIRED. This is how we initialize the widget. You have several options you can configure. Read them over for more information. -->
    <script>
      var cio_results = Elm.Results.init(
        {
          node: document.getElementById("cio_results"), // REQUIRED. Must match the ID of the div we're replacing, which is "results" in this example.
          flags: {
            subdomain: "nl", // REQUIRED. This is your club's Curling I/O subdomain. For example, if your Curling I/O URL begins with "demo.curling.io" then the "demo" part would be your subdomain.
            section: "competitions", // OPTIONAL. Can be "leagues", "competitions", or "products". Will default to "leagues" if omitted or an invalid value is passed.
            fullScreenToggle: true, // OPTIONAL. Determines if the full screen toggle (two diagonal arrows in the top right) will be shown. Will default to false if omitted or an invalid value is passed.
            registration: false, // OPTIONAL. Set to false if you don't want prices and the add to cart / register / waitlist buttons to show up.
            showWaiversForTeams: false, // OPTIONAL. Set to true if you want the team pages to show which waivers a curler has agreed to or None if they haven't.
            // eventId: 3742, // OPTIONAL. If you only want to show one specific event, enter it's ID here.
            excludeEventSections: ["details", "registrations", "spares"], // OPTIONAL. Event sections you don't want to show up. Possible values: "details", "registrations", "spares", "draws", "stages", "teams", "reports"
            defaultEventSection: "draws", // OPTIONAL. If you want a default event section other than the details view. Possible values: "registrations", "spares", "draws", "stages", "teams", "reports"
            theme: { // OPTIONAL. You can customize the colors used.
              primary: "#ed1940", // OPTIONAL. The primary color in hexadecimal (important buttons / links / backgrounds). Default is red: #ed1940
              secondary: "#5c5c5c" // OPTIONAL. The secondary color in hexadecimal (minor buttons / links / backgrounds). Default is a dark grey: #5c5c5c
            },
            // lang: "en", // OPTIONAL. Options are "en" or "fr". Defaults to "en" if nothing is passed. If your using wordpress, it should expose a 2 letter language code that can be passed here.
            host: document.location.host, // REQUIRED - DO NOT MODIFY. Let's us make slight behavioural changes when hosted offsite versus within your curling.io site.
            hash: document.location.hash, // REQUIRED - DO NOT MODIFY. This will allow users to bookmark and share specific event links.
          }
        }
      )

      // REQUIRED - DO NOT MODIFY. Used for navigation to hopfully prevent third party script interference.
      cio_results.ports.navigateTo.subscribe(function(newHash) {
        document.location.hash = newHash
      })

      // REQUIRED - DO NOT MODIFY. Used for navigation.
      addEventListener("hashchange", (event) => {
        cio_results.ports.hashChangeReceiver.send(location.hash)
      })
    </script>


## Current Games Widget

The current games widget is meant to be embedded in a sidebar on the home page.
It will list only the current games that are being played across all of your competitions with their scores.

Place the following raw javascript code into your sidebar widget to display the current games widget:

    <!-- REQUIRED. This script tag is required to load the widget. -->
    <!-- Ideally this goes in the head of your html document, but you can play around to see what works (depending on your CMS) -->
    <script src="https://pairshaped.github.io/curlingio-results/current-games-prod.min.js"></script>

    <!-- REQUIRED. This is the div the widget replaces. The ID of this div must match the getElemenetById below that's passed to the node. If you change the ID here, make sure you also change it down there. -->
    <div id="cio_current_games"></div>

    <!-- REQUIRED. This is how we initialize the widget. You have several options you can configure. Read them over for more information. -->
    <script>
      Elm.CurrentGames.init(
        {
          node: document.getElementById("cio_current_games"), // REQUIRED. Must match the ID of the div we're replacing, which is "results" in this example.
          flags: {
            subdomain: "nl", // REQUIRED. This is your club's Curling I/O subdomain. For example, if your Curling I/O URL begins with "demo.curling.io" then the "demo" part would be your subdomain.
            // lang: "en", // OPTIONAL. Options are "en" or "fr". Defaults to "en" if nothing is passed. If your using wordpress, it should expose a 2 letter language code that can be passed here.
            theme: { // OPTIONAL. You can customize the colors used.
              primary: "#ed1940", // OPTIONAL. The primary color in hexadecimal (important buttons / links / backgrounds). Default is red: #ed1940
              secondary: "#5c5c5c" // OPTIONAL. The secondary color in hexadecimal (minor buttons / links / backgrounds). Default is a dark grey: #5c5c5c
            },
            host: document.location.host // REQUIRED - DO NOT MODIFY. Let's us make slight behavioural changes when hosted offsite versus within your curling.io site.
          }
        }
      )
    </script>


## Uninstall Legacy Widgets

Once you have completed the V2 widget integrations above, please uninstall and remove all references to the Curling I/O legacy plugin.
