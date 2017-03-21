// Modulo de google-maps
// =====================

// se puede usar en cualquier otro módulo de
// angular usando:
//   angular.module('otro', [ 'google-maps']);

// modulo: google-maps
angular.module("google-maps", []);

// servicio: googleMaps
angular.module("google-maps")
  .factory("googleMaps", function($rootScope) {

    var factory = {};

    // conjunto de marcadores
    factory.markers = [];

    // marcador seleccionado, se coloca en el centro del mapa
    factory.selectedMarkerIdx = null;

    // dimensiones del mapa
    factory.mapWidth = 0;


    /**
     * Inicializa el mapa
     * @param  {HTMLElement} elem - elemento HTML que contiene el mapa
     * @param  {array} opciones para Google Maps
     * @return {object} - instancia de map
     */
    factory.initializeMap = function(elem, options) {
      // inicializa las opciones si no llegan como parámetro
      options = options || {
        zoom: 4,
        center: new google.maps.LatLng(4.6015857, -74.06527449999999),
        mapTypeId: google.maps.MapTypeId.ROADMAP,
        zoomControlOptions: {
          position: google.maps.ControlPosition.RIGHT_BOTTOM
        },
        panControlOptions: {
          position: google.maps.ControlPosition.RIGHT_BOTTOM
        }
      };
      // si ya existe un mapa
      if (this.map) {
        // elimina el mapa anterior e inicializa el marcador seleccionado
        delete this.map;
        this.selectedMarkerIdx = null;
      }
      // crea el mapa usando el API de Google Maps
      var map = this.map = new google.maps.Map(elem, options);

      // obtiene el tamaño del mapa en pantalla
      // solo se actualiza si hay datos
      if (elem.clientWidth > 0) {
        this.mapWidth = elem.clientWidth;
      }
      // retorna el mapa
      return map;
    };


    /**
     * Obtiene un objeto Geocoder de Google
     * @return {Geocoder} - instancia de Geocoder
     */
    factory.getGeoCoder = function() {
      return new google.maps.Geocoder();
    };

    /**
     * Coloca marcadores en el mapa
     * @param  {array} datos de los marcadores
     * @return {void}
     */
    factory.placeMarkers = function(data) {
      this.clearAllMarkers();
      var me = this,
        bounds = new google.maps.LatLngBounds();
      var count = 1;
      angular.forEach(data, function(item, key) {
        console.log(item);
        var latLng = new google.maps.LatLng(item.geometry.location.lat(), item.geometry.location.lng()),
          currentMarker;
        me.markers.push(currentMarker = new google.maps.Marker({
          map: me.map,
          position: latLng,
          animation: google.maps.Animation.DROP,
        }));
        bounds.extend(latLng);
        google.maps.event.addListener(currentMarker, "click", function() {
          me.selectedMarkerIdx = key;
          $rootScope.$apply();
        });
      });
      me.map.fitBounds(bounds);
    };

    /**
     * Limpia/Elimina los marcadores del mapa
     * @return {void}
     */
    factory.clearAllMarkers = function() {
      angular.forEach(this.markers, function(item, key) {
        item.setMap(null);
      });
      this.markers = [];
    };

    /**
     * Hace zoom sobre un marcador específico en el mapa
     * @param  {integer} índice del marcador
     * @return {void}
     */
    factory.zoomToMarker = function(idx) {
      // Obtiene el marcador en la posición especificada
      var p = this.markers[idx].getPosition();
      // coloca el marcador en el centro del mapa
      this.map.setCenter(p);
      // cambia el zomm a nivel 16
      this.map.setZoom(16);
    };

    return factory;

  });


// directiva: gmap
// ej:   <div ... gmap="" data="data">
//       data es un arreglo con los datos de los marcadores
angular.module("google-maps")
  .directive("gmap", ["googleMaps",
    function(googleMap) {

      return {
        restrict: "EA",
        scope: {
          data: "=data"
        },
        link: function(scope, elem, attrs) {

          // inicializa el mapa en el elemento (p.ej. un div)
          // señalado por la directiva "gmap"
          var places = [{
            lat: 4.7837556,
            lng: -74.0441833
          }, {
            lat: 4.7103500,
            lng: -74.0304167
          }];

          var map = googleMap.initializeMap(elem[0]),
            markers = [];

          /**
           * coloca los marcadores en el mapa
           * @param  {array} arreglo con los datos de marcadores
           * @return {void}
           */
          var renderMap = function(mapData) {
            if (!mapData) {
              return;
            }
            googleMap.placeMarkers(mapData);
          };

        }
      };
    }
  ]);