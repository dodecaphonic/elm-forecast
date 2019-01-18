const express = require("express");
const dotenv = require("dotenv");
const sprintf = require("sprintf-js").sprintf;
const fetch = require("node-fetch");

if (process.env.NODE_ENV !== "production") {
  dotenv.config();
}

const DARK_SKY_API = `https://api.forecast.io/forecast/${
  process.env["DARK_SKY_API_TOKEN"]
}/%f,%f?units=ca`;

const GOOGLE_API = `https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=${
  process.env["GOOGLE_API_TOKEN"]
}"`;

function fetchGeocoding(address) {
  return fetch(sprintf(GOOGLE_API, address)).then(r => r.json());
}

function fetchForecast(lat, lon) {
  return fetch(sprintf(DARK_SKY_API, lat, lon)).then(r => r.json());
}

function main() {
  const app = express();
  const port = parseInt(process.env.PORT || "9292", 10);

  app.get("/fetch/:lat/:lon", async (req, res) => {
    res.set("Access-Control-Allow-Origin", "*");

    const forecast = await fetchForecast(
      parseFloat(req.params.lat),
      parseFloat(req.params.lon)
    );

    res.send(JSON.stringify(forecast));
  });

  app.get("/geocode", async (req, res) => {
    res.set("Access-Control-Allow-Origin", "*");

    const geocodingResults = await fetchGeocoding(req.params.address);
    res.send(JSON.stringify(geocodingResults));
  });

  app.listen(port, () => {
    console.log(`DarkSky + Geocoding proxy running on port ${port}`);
  });
}

main();
