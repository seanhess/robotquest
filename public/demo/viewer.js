
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

        if (locations) 
            draw(locations)
    }


    function drawLocation(loc, color,  blockWidth, blockHeight) {
        var x = loc.point.x * blockWidth
        var y = loc.point.y * blockHeight 
        ctx.fillStyle = color;
        ctx.fillRect(x, y, blockWidth, blockHeight)
    }

    function draw(newLocations) {
        locations = newLocations

        ctx.fillStyle = "#FFF"
        ctx.fillRect(0, 0, canvas.width, canvas.height)
        // ctx.clearRect(0, 0, canvas.width, canvas.height)
        for (var i = 0; i < locations.length; i++) {
            drawLocation(locations[i], '#000', blockWidth, blockHeight)
        }
    }

    $window.bind('resize', resize)
    resize()

    this.draw = draw
}
