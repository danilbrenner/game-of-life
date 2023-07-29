# Conway's game of life

See rules [here](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)

See running app [here](https://golhost.z16.web.core.windows.net/)

## Develop

### Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 5.0 or higher
* [node.js](https://nodejs.org)
* An F# editor like Visual Studio, Visual Studio Code with [Ionide](http://ionide.io/) or [JetBrains Rider](https://www.jetbrains.com/rider/)

### Building and running the app

* Install dependencies: `npm install`
* Start the compiler in watch mode and a development server: `npm start`
* After the first compilation is finished, in your browser open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.

### Bundling for release

Run the following command to compile and bundle up all your F# code into one Javascript file: `npm run build`. The compiled output ends up in the `public` folder under the name `bundle.js`.

### Run unit tests

* Run unit tests continuously `dotnet watch run --project tests/GamaOfLife.UnitTests/GamaOfLife.UnitTests.fsproj`
