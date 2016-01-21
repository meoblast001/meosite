$(document).ready ->
  map = new OpenLayers.Map("mapdiv")
  map.addLayer(new OpenLayers.Layer.OSM())

  zoom = 3

  xml = $($.parseXML($('#locations').text()))
  home = xml.find('home')
  locations = xml.find('location')

  createLonLat = (xml_node) ->
    longitude = parseFloat($(xml_node).attr('longitude'))
    latitude = parseFloat($(xml_node).attr('latitude'))

    lon_lat = new OpenLayers.LonLat(longitude, latitude)
      .transform(#Transform from WGS 1984.
                 new OpenLayers.Projection("EPSG:4326"),
                 #To Spherical Mercator Projection.
                 map.getProjectionObject())

  markers = new OpenLayers.Layer.Markers("Places to Which I've Been")
  map.addLayer(markers)

  home_lon_lat = createLonLat(home)
  markers.addMarker(new OpenLayers.Marker(home_lon_lat))
  map.setCenter(home_lon_lat, zoom)

  for location in locations
    lon_lat = createLonLat($(location))
    markers.addMarker(new OpenLayers.Marker(lon_lat))
