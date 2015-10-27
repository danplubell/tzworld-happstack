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

# License
The MIT License (MIT)

Copyright (c) 2015 Dan Plubell <danplubell@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

