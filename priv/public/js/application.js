
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
            strokeWeight: 2
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
        zoom: 13,
        mapTypeId: google.maps.MapTypeId.ROADMAP,
        center: this.center
    });

    this.client = $.bullet('ws://localhost:8080/stream');
    this.client.onopen = function() {
        var route = [
            100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
            26, 27, 28, 29, 30, 31, 32, 33, 34, 37, 38, 39, 40, 41, 42, 43, 44, 45,
            46, 47, 48, 49, 5, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62,
            63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 80, 81,
            82, 83, 85, 87, 88, 89, 90, 91, 94, 95, 96, 97, 98, 99
	];

        for(var i = 0; i < route.length; ++i) {
            self.subscribe(route[i]);
        }
    };
    this.client.onmessage = function(evt) {
        return self.onmessage(evt);
    };

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
        if(!this.tracks[sensorId]) {
            this.tracks[sensorId] = new Track(sensorId, this.map);
        }
        this.send({ mode: 'subscribe', sensor_id: sensorId });
    },

    onmessage: function(evt) {
        data = JSON.parse(evt.data);
        this.tracks[data.name].update(data.data);
    }

};
