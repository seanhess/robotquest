
function viewer(id) {
    var $window = $(window) 
    var $canvas = $(id)
    var canvas = $canvas.get(0)
    var ctx = canvas.getContext("2d")
    var PollInterval = 1000 

    var blockWidth 
    var blockHeight

    var offset = $canvas.position().left
    var factor = 1

    // resize
    function resize(size) {
        // console.log("SIZE", $window.height(), $window.width())
        // use aspect-ratio sizing. Size to height, which will always be limiting factor
        // canvas.width = $window.width()

        var totalWidth = $window.width()
        var totalHeight = $window.height()

        factor = Math.floor(totalHeight / size.height)
        console.log("NEW FACTOR " + factor)

        canvas.width = size.width * factor
        canvas.height = size.height * factor

        blockWidth = factor
        blockHeight = factor
    }

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

        // drawLocation({point: {x: 0, y: 0}}, 10, 10)

        getWorldInfo(function(start, size) {
            var interval = setInterval(function() {
                getWorld(function(locations) {
                    ctx.clearRect(0, 0, canvas.width, canvas.height)
                    for (var i = 0; i < locations.length; i++) {
                        drawLocation(locations[i], blockWidth, blockHeight)
                    }
                })
            }, PollInterval)

            $window.bind('resize', function() {
                resize(size)
            })

            resize(size)
        })
    }

    return {
        start: start
    }
}
