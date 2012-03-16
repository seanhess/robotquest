
// deps on api.js for "call"

// Bots create
function ChaosBot(worldInfo, color) {

    color = color || "#000" 

    var unitId, point, unitToken, currentAction
    var currentAction = randomAction()

    this.tick = tick
    this.start = start

    var description = {kind: "ChaosBot", name: "ChaosBotN", source: "http://github.com/seanhess/botland", notes:"chaos bot moves in a direction until hitting something, then changes directions", color: color } 

    // we ignore the world. Easier to rely on failures
    function tick(world, units) {
        var newPoint = currentAction(point)
        request("POST", "/units/" + unitId + "/move", newPoint, unitToken, function(err) {

            // this probably means we hit something. Change actions
            if (err)
                currentAction = randomAction()

            // don't update the point unless we moved successfully
            else
                point = newPoint
        })
    }

    function start() {
        request("POST", "/units", {requestedPoint: randomPoint(worldInfo.fieldSize), unitDescription: description}, "", function(err, spawn) {

            if (err) throw err

            unitId = spawn.unitId
            point = spawn.spawnPoint
            unitToken = spawn.unitToken

        })
    }

    function left(point) {
        return {x: point.x - 1, y: point.y}
    }

    function right(point) {
        return {x: point.x + 1, y: point.y}
    }

    function up(point) {
        return {x: point.x, y: point.y - 1}
    }

    function down(point) {
        return {x: point.x, y: point.y + 1}
    }

    function randomAction() {
        var actions = [left, right, up, down]
        var index = Math.floor(Math.random() * actions.length)
        return actions[index]
    }
}