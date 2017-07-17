#Conway's game of life

My simple implementation of Conway's game of life using f# and fable compiler.

* You can read about Conway's game of life on [Wikipedia](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)
* Or paly this game [here.](http://gol.codingape.net/)

## Build and running the app

1. Install npm dependencies: `yarn install`
2. Install dotnet dependencies: `dotnet restore`
3. Start Fable server and Webpack dev server: `dotnet fable npm-run start`
   or `dotnet fable npm-run build` to build docker image
4. In your browser, open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.
