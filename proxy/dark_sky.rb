require "bundler/setup"

require "cuba"
require "httparty"
require "uri"
require "dotenv"
Dotenv.load

DARK_SKY_API = "https://api.forecast.io/forecast/#{ENV["DARK_SKY_API_TOKEN"]}/%f,%f?units=ca"
GOOGLE_API_TOKEN = "https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=#{ENV["GOOGLE_API_TOKEN"]}"

Cuba.define do
  on get do
    on "fetch/:lat/:lon" do |lat, lon|
      res["Access-Control-Allow-Origin"] = "*"
      res.write fetch_forecast(lat, lon)
    end

    on "geocode" do
      res["Access-Control-Allow-Origin"] = "*"
      res.write fetch_geocoding(req.params["address"])
    end
  end
end

def fetch_forecast(lat, lon)
  HTTParty.get(format(DARK_SKY_API, lat, lon)).to_json
  # File.read(File.expand_path("forecast.json", __dir__))
end

def fetch_geocoding(address)
  # HTTParty.get(format(GOOGLE_API_TOKEN, URI.encode(address))).to_json
  File.read(File.expand_path("geocoding.json", __dir__))
end
