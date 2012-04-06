
$(function() {
    var bots = []

    var $window = $(window)
    var $viewer = $("#viewer")
    var $container = $("#container")

    var PX = 32
    var PY = 32

    var padding = 0

    $window.on('resize', function() {
        console.log("RESIZE")
    })

    // first, get the world
    $.get("/game", function(gameInfo) {

        var w = gameInfo.width
        var h = gameInfo.height
        var ms = gameInfo.tick

        // this will make it scroll
        $viewer.css({width: w * PX, height: h * PY})
        $container.css({width: (w+2)*PX, height: (h+2)*PY})

        console.log("GAME INFO", w, h, ms)

        function poll() {
            $.get("/game/minions", function(bots) {
                tick(bots)
            })
        }

        function tick(bots) {

            $(".bot").addClass("inactive")

            bots.forEach(function(bot) {
                var $bot = $("#" + bot.id)

                if (!$bot.length) {
                    console.log("NEW BOT", bot.id, bot.name, bot.x, bot.y)
                    $bot = botView(bot)
                    $viewer.append($bot)
                }

                $bot.css({left: (bot.x) * PX + padding, top: (bot.y) * PY + padding})
                // $bot.css({width: PX, height: PY})
                $bot.removeClass("inactive")
            })

            $(".bot.inactive").remove()
        }

        function botView(bot) {
            var $bot = $("<a href='/bots/"+bot.id+"' class='bot sprite undead' id='"+bot.id+"'></a>")
            $bot.data(bot)
            // compute offset mathematically
            // dragon-3-4
            var xi = 3
            var yi = 4
            $bot.css({'background-position': '-' + xi*32 + 'px -' + yi*32 + 'px'})
            return $bot
        }

        poll()
        setInterval(poll, ms)
    })




    // require moment.js
    function age(created) {
        return moment(created).fromNow().replace(" ago", "")
    }

    // BOT INFORMATION
    var $botInfo = $("#botInfo")

    function showBot(bot) {
        $botInfo.show()
        $botInfo.find(".name").text(bot.name)
        $botInfo.find(".age").text(age(bot.created))
        $botInfo.find(".kills").text(bot.kills)
        $botInfo.find(".player").text(bot.player)
        $botInfo.find(".sprite").addClass('monster1').css({'background-position': '0px 0px'})

        // get player information
        $.get('/players/' + bot.player, function(player) {
            $botInfo.find(".source").attr('href', player.source)
        })

        // TODO get player information, add it hereszzz
        // TODO change bot: player (drop source)
    }

    $viewer.on("click", ".bot", function(event) {
        showBot($(this).data())
        return false
    })

    // only outside clicks
    $botInfo.on("click", function(e) {
        $botInfo.hide()
    })

})
