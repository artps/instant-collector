
var Track = function(sensorId, map) {
    var self = this;
    this.marker = null;
    $.get('/data/' + sensorId +'.json', function(response) {
        var pathCoords = [];
        for(var i = 0; i < response.data.length; ++i) {
            coords = response.data[i];
            var position = new google.maps.LatLng(coords.lat, coords.long);
            if(i == 0) {
                self.marker = new google.maps.Marker({
                    map: map,
                    position: position
                });
            }
            pathCoords.push(position);
        }

        var path = new google.maps.Polyline({
            path: pathCoords,
            strokeColor: self.color(),
            strokeOpacity: 1.0,
            strokeWeight: 1
        });

        path.setMap(map);
    });
};

Track.prototype = {
    update: function(data) {
        var position = new google.maps.LatLng(data.lat, data.long);
        this.marker.setPosition(position);
    },
    color: function() {
        return '#' + Math.floor(Math.random()*16777215).toString(16);
    }
};

var App = function() {
    var self = this;

    this.center = new google.maps.LatLng(55.8005556, 49.1055556);

    this.map = new google.maps.Map(document.getElementById('canvas'), {
        zoom: 11,
        mapTypeId: google.maps.MapTypeId.ROADMAP,
        center: this.center
    });

    this.client = new SockJS('/channel');
    this.client.onopen = function() {
        console.log('open');
    };
    this.client.onmessage = function(evt) {
        return self.onmessage(evt);
    };
    this.client.onclose = function() { };

    this.tracks = {};
};

App.init = function() {
    App.instance = new App();
    return App.instance;
};

App.subscribe = function(sensorId) {
    App.instance.subscribe(sensorId);
};

App.unsubscribe = function(sensorId) {
    App.instance.unsubscribe(sensorId);
};

App.prototype = {

    send: function(data) {
        this.client.send(JSON.stringify(data));
    },

    subscribe: function(sensorId) {
        this.tracks[sensorId] = new Track(sensorId, this.map);
        this.send({ mode: 'subscribe', sensor_id: sensorId });
    },

    onmessage: function(evt) {
        data = JSON.parse(evt.data);
        this.tracks[data.name].update(data.data);
    }

};
