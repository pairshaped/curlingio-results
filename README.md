# Results Widget for [Curling I/O](https://curling.io)


## Overview

Embeddable javscript widget to display a curling club's leagues, competitions, and products pulled from the Curling I/O API.

For leagues and competitions it will display brakcets, standings, schedule, scores, end scores, and shot by shot results as relevant.

See the example.html file in this directory for an example of how you can include this widgets on your own club website.

## Using the widget on your club's website

You can review the [example.html file](example.html) for an example of how to embed this widget on your curling club's website.
You can save this file locally and open it in a browser to mess around with the settings and see the results.

### 1. Include the Widget's Javascript

You'll need to include the widget's javascript.
```<script src="https://pairshaped.github.io/curlingio-results/prod.min.js"></script>```

### 2. Add the placeholder div to your page where you want the widget to be inserted.

```<div id="curlingio_results"></div>```

It's important that the ID here is the same used in the next step.

### 3. Configure and Load the Widget

```
<script>
  // REQUIRED. This is how we initialize the widget. You have several options you can configure. Read them over for more information.
  var scoring = Elm.Results.init(
    {
      node: document.getElementById("curlingio_results"), // REQUIRED. Must match the ID of the div we're replacing, which is "results" in this example.
      flags: {
        subdomain: "demo", // REQUIRED. This is your club's Curling I/O subdomain. For example, if your Curling I/O URL begins with "demo.curling.io" then the "demo" part would be your subdomain.
        fullScreenToggle: true, // OPTIONAL. Determines if the full screen toggle (two diagonal arrows in the top right) will be shown. Will default to false if omitted or an invalid value is passed.
        section: "leagues", // OPTIONAL. Can be "leagues", "competition", or "products". Will default to "leagues" if omitted or an invalid value is passed.
        registration: true, // OPTIONAL. Set to false if you don't want prices and the add to cart / register / waitlist buttons to show up.
        // eventId: 3742, // OPTIONAL. If you only want to show one specific event, enter it's ID here.
        // excludeEventSections: ["details", "registrations"], // OPTIONAL. Event sections you don't want to show up. Possible values: "details", "registrations", "draws", "stages", "teams", "reports"
        // defaultEventSection: "draws", // OPTIONAL. If you want a default event section other than the details view. Possible values: "registrations", "spares", "draws", "stages", "teams", "reports"
        lang: "en", // OPTIONAL. Options are "en" or "fr". Defaults to "en" if nothing is passed. If your using wordpress, it should expose a 2 letter language code that can be passed here.
        host: document.location.host, // REQUIRED - DO NOT MODIFY. Let's us make slight behavioural changes when hosted offsite versus within your curling.io site.
        hash: document.location.hash, // REQUIRED - DO NOT MODIFY. This will allow users to bookmark and share specific event links.
      }
    }
  )

  // REQUIRED - DO NOT MODIFY. Used for navigation to hopfully prevent third party script interference.
  scoring.ports.navigateTo.subscribe(function(newHash) {
    document.location.hash = newHash
  })

  // REQUIRED - DO NOT MODIFY. Used for navigation.
  addEventListener("hashchange", (event) => {
    scoring.ports.hashChangeReceiver.send(location.hash)
  })
</script>
```

Please review the comments in the embedded code to make configuration tweaks. Here are some examples of configuration changes and their effects:

1. You can choose which list view to show; leagues, competitions, or products.
2. You can include just the screens for a specific league or competition, instead of the listing views. Like if you're promoting a specific bonspiel on it's own page.
3. You can enable / disable the registration buttons for your leagues, competitions, and products. Like if you just want to display the results for your competitions.
4. You can exclude / disable specific sections. Like if you don't want the details section (tab) to show up.
5. You can specify which section should be the default for your leagues and competitions.


## For Contributors

### Installing Dependencies

We use elm and elm-live for development. You can install these via npm.

```
npm install
```

### Running It

Edit dev.html and configure the application's parameters for your environment. Then run it:

```
npm start
```

### Production Deployment

Make sure you have uglify-js installed to compress the production js.
```
npm install -g uglify-js
```

Compile and optimize for production using:

```
./prod.sh
```

## Copyright and License

[See LICENSE.md](LICENSE.md)
