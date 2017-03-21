var app = angular.module('calidadAire', ["google-maps"]);
app.controller("MainCtrl", ['$scope', '$location', "googleMaps", function($scope, $location, googleMap) {

  // place : nombre del lugar a buscar
  $scope.place = "Universidad de los Andes, Bogotá, Colombia";

  // search() : busca el lugar y actualiza el mapa
  $scope.search = function(place) {
    console.log(place);
    console.log("Iteracion");

    // ejecuta geocode
    googleMap.getGeoCoder().geocode({
      location: place

    }, function(results, status) {
      // muestra en consola el primer resultado
      var lat = results[0].geometry.location.lat(),
        lng = results[0].geometry.location.lng();
      console.log(lat, lng);
      // usa $scope.$apply() debido a que esta función se ejecuta
      // en el alcance del servicio "google-maps". Al ejecutar 
      // $apply, el controlador es notificado de los cambios


      var marker = new google.maps.Marker({
        position: {
          lat: lat,
          lng: lng
        },
        title: "Hello World!"
      });
      // To add the marker to the map, call setMap();
    });
  };
  var marker = new google.maps.Marker({
    position: {
      map: google.map,
      lat: 4.7837556,
      lng: -74.0441833
    },
    title: "Guaymaral"
  });
   marker.setMap(google.map);

}]);