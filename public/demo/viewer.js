
function viewer(id) {
    var $canvas = $(id)
    var canvas = $canvas.get(0)
    var ctx = canvas.getContext("2d")
    var cw = canvas.width
    var ch = canvas.height
    var PollInterval = 1000 

    function getWorldInfo(cb) {
        $.get("/world", function(info) {
            cb(info.fieldStart, info.fieldSize)
        })
    }

    function getWorld(cb) {
        $.get("/world/locations", cb)
    }

    function drawLocation(loc, blockWidth, blockHeight) {
        var x = loc.point.x * blockWidth
        var y = loc.point.y * blockHeight 
        ctx.fillStyle = "#000"
        ctx.fillRect(x, y, blockWidth, blockHeight)
    }

    function start() {
        console.log("START")
        getWorldInfo(function(start, size) {
            var blockWidth = cw / size.width
            var blockHeight = ch / size.height
            var interval = setInterval(function() {
                getWorld(function(locations) {
                    ctx.clearRect(0, 0, cw, ch)
                    for (var i = 0; i < locations.length; i++) {
                        drawLocation(locations[i], blockWidth, blockHeight)
                    }
                })
            }, PollInterval)
        })
    }

    return {
        start: start
    }
}
