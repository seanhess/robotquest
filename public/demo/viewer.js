
function Viewer($parent, size) {
    this.$parent = $parent
    var $window = $(window) 
    var $canvas = $('<canvas class="viewer" width="100" height="100"></canvas>')
    $parent.append($canvas)

    var canvas = $canvas.get(0)
    var ctx = canvas.getContext("2d")

    var blockWidth 
    var blockHeight

    var offset = $canvas.position().left
    var factor = 1

    var locations
    var units

    // resize
    function resize() {
        // console.log("SIZE", $window.height(), $window.width())
        // use aspect-ratio sizing. Size to height, which will always be limiting factor
        // canvas.width = $window.width()

        var totalWidth = $parent.width()
        var totalHeight = $parent.height()

        factor = Math.floor(totalHeight / size.height)

        canvas.width = size.width * factor
        canvas.height = size.height * factor

        blockWidth = factor
        blockHeight = factor

        if (locations && units) 
            draw(locations, units)
    }


    function drawLocation(point, description, blockWidth, blockHeight) {

        // wait until it's fully available to draw it
        if (!description.color) return

        var x = point.x * blockWidth
        var y = point.y * blockHeight 


        ctx.fillStyle = description.color;
        ctx.strokeStyle = "#000"

        // ctx.fillRect(x, y, blockWidth, blockHeight)
        // ctx.strokeRect(x, y, blockWidth, blockHeight)
        var padding = 1
        var r = blockWidth / 2 - 2 * padding
        ctx.beginPath() 
        ctx.arc(x + r + padding, y + r + padding, r, 0, Math.PI * 2)
        ctx.closePath()
        ctx.stroke()
        ctx.fill()
    }

    function draw(newLocations, newUnits) {
        locations = newLocations
        units = newUnits

        // clear the whole board
        ctx.fillStyle = "#FFF"
        ctx.fillRect(0, 0, canvas.width, canvas.height)
        // ctx.clearRect(0, 0, canvas.width, canvas.height)

        // draw all locations
        for (var i = 0; i < locations.length; i++) {
            var location = locations[i]
            var point = location.point
            var unitId = location.unitId
            var description = units[unitId]
            drawLocation(point, description, blockWidth, blockHeight)
        }
    }

    $window.bind('resize', resize)
    resize()

    this.draw = draw
}
