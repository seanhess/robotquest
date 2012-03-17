
// deps on api.js for "call"

// Bots create
function CleanupBot(worldInfo) {

    // ooh! I could have a sigla field
    // it can't change, must be transparent Black, or something like that. 

    var color = "#888" // ignore the color you pick. Helps identify these guys

    var unitId, unitToken, point, oldMap = {}

    this.tick = tick
    this.start = start

    var description = {kind: "CleanupBot", name: "CleanupBotN", source: "http://github.com/seanhess/botland", notes:"moves towards inactive units and destroys them", color: color } 

    function tick(locations, units) {

        var map = makeMap(locations)

        if (map[pointId(point)] != unitId) {
            throw new Error("Incorrect position")
        }

        // scan for inactive things
        var inactive = []
        for (var x = 0; x < worldInfo.fieldSize.width; x++) {
            for (var y = 0; y < worldInfo.fieldSize.height; y++) {
                var p = {x:x, y:y}
                var pid = pointId(p)

                if (map[pid] && map[pid] == oldMap[pid] && !(p.x == point.x && p.y == point.y)) {
                    inactive.push(p)
                    // var d = distance(point, p)
                    // if (!closest || d < closest.d) {
                    //     // console.log("OLD CLOSE", JSON.stringify(closest))
                    //     closest = {point: p, distance: d}
                    //     console.log("NEW CLOSE", JSON.stringify(p), d)
                    // }
                }
            }
        }

        oldMap = map

        inactive = inactive.map(function(p) {
            return {point: p, distance: distance(point, p)}
        })

        inactive = inactive.sort(function(a, b) {
            return a.distance - b.distance
        })

        function distance(a, b) {
            return Math.abs(a.x - b.x) + Math.abs(a.y - b.y) 
        }

        var target = inactive[0]
        // console.log("CHOSE TARGET", JSON.stringify(target))
        // console.log("ALL INACTIVES", JSON.stringify(inactive))

        // now, move towards inactive!

        // try to kill

        if (!target) return // wait around for an inactive to show up

        if (target) {
            // console.log("TARGET", JSON.stringify(point), JSON.stringify(target))

            if (target.distance < 2) {
                // console.log("ATTACK!")
                request("POST", "/units/" + unitId + "/attack", target.point, unitToken, function(err) {
                    if (err) throw err
                    // console.log("ATTACK SUCCESSFUL")
                })
            }

            else if (target.distance == 0) {
                // console.log("SKIP SELF")
            }

            // move closer
            else {
                var dx = point.x - target.point.x 
                var dy = point.y - target.point.y

                var newPoint;

                if (point.x < target.point.x)
                    newPoint = {x: point.x + 1, y: point.y} 

                else if (point.x > target.point.x)
                    newPoint = {x: point.x - 1, y: point.y}

                else if (point.y < target.point.y)
                    newPoint = {x: point.x, y: point.y + 1}

                else 
                    newPoint = {x: point.x, y: point.y - 1}

                request("POST", "/units/" + unitId + "/move", newPoint, unitToken, function(err) {

                    // we probably hit something. Just wait for the next tick
                    if (err) return
                    else point = newPoint // update our location
                })

            }
        }

    }

    // point -> unitId
    function makeMap(locations) {
        var map = {}
        locations.forEach(function(l) {
            map[pointId(l.point)] = l.unitId
        })
        return map
    }

    function pointId(p) {
        return p.x + "," + p.y
    }

    function isInactive(unitId, world, oldWorld) {

    }

    function start() {
        request("POST", "/units", {requestedPoint: randomPoint(worldInfo.fieldSize), unitDescription: description}, "", function(err, spawn) {

            if (err) throw err

            unitId = spawn.unitId
            unitToken = spawn.unitToken
            point = spawn.spawnPoint
        })
    }
}