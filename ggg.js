// example ggg.js. Delete what you don't need
module.exports = {

  install: "echo 'nothing'",

  // servers to deploy to
  servers: {
    api: {
      hosts: "root@robotquest.tk",
      start: "./Api",
      install: ""
    },
    ai: {
      hosts: "root@robotquest.tk",
      start: "node_modules/.bin/coffee ai/app.coffee",
      install: "npm install"
    }
  }
}
