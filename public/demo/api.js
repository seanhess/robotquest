function request(method, url, body, authToken, cb) {

    var b = JSON.stringify(body)
    $.ajax( { type: method
            , url: url
            , contentType: 'application/json'
            , data: b
            , dataType: "json"
            , headers: {"X-Auth-Token":authToken}
            , error: function(xhr) {
                cb(new Error(xhr.responseText))
            }
            , success: function(data) {
                cb(null, data)
            }
        })
}

function randomPoint(size) {
    return {x: random(size.width), y: random(size.height)}
}

function random(max) {
    return Math.floor(Math.random() * max)
}