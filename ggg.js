// example ggg.js. Delete what you don't need
module.exports = {

  install: "echo 'nothing'",

  // servers to deploy to
  servers: {
    api: {
      hosts: "root@184.106.251.159",
      start: "./Api",
      install: ""
    },
    ai: {
      hosts: "root@184.106.251.159",
      start: "node_modules/.bin/coffee ai/app.coffee",
      install: "npm install"
    }
  }
}
