# tzworld-happstack
This project is Happstack implementation of a web service that provides a mechanism for looking up an Olson time zone for a location by the latitude and longitude.

It uses tzworld-api for the lookup and for the request handler.

API
This service support one route:

`http://<your base>/location?lat=<your latitude>&lon=<your longitude> `

If the query is successful you will receive the follow json structure and a 200 HTTP status code:

`{

"tzlongitude":-86.622625, #This is the longitude you provided in the request

"tzfound":true, #This is true if a time zone was found

"tzlatitude":41.294159, #This is the latitude you provided

"tzname":"America/Indiana/Knox #This is the Olson name of the timezone that was found. 

"}`

If there is a problem then you will receive the following json message structure and a 400 HTTP Status code:

`{"message":<message that describes the error>}`

This service is subject to the limitations of the data used by tzworld-api.

