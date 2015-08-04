require "bundler/setup"

require "cuba"
require "httparty"
require "dotenv"
Dotenv.load

DARK_SKY_API = "https://api.forecast.io/forecast/#{ENV["DARK_SKY_API_TOKEN"]}/%f,%f?units=ca"

Cuba.define do
  on get do
    on "fetch/:lat/:lon" do |lat, lon|
      res["Access-Control-Allow-Origin"] = "*"
      res.write fetch(lat, lon)
    end
  end
end

def fetch(lat, lon)
  # HTTParty.get(format(DARK_SKY_API, lat, lon)).to_json
  File.read(File.expand_path("forecast.json", __dir__))
end
