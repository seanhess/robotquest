// example ggg.js. Delete what you don't need
module.exports = {

  // services
  start: "node app.js",

  // install
  install: "npm install",

  // servers to deploy to
  servers: {
    api: {
      hosts: "root@199.175.48.195",
      start: "cabal-dev/bin/botland",
      install: "bin/install"
    },
    ai: {
      hosts: "root@199.175.48.195",
      start: "node_modules/.bin/coffee ai/app.coffee",
      install: "npm install"
    }
  }
}
