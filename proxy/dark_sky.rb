require "cuba"
require "httparty"

DARK_SKY_API = "https://api.forecast.io/forecast/#{ENV["DARK_SKY_API_TOKEN"]}/"

Cuba.define do
  on get do
    on "fetch/:lat/:lon" do |lat, lon|
      res["Access-Control-Allow-Origin"] = "*"
      res.write fetch(lat, lon)
    end
  end
end

def fetch(lat, lon)
  File.read(File.expand_path("forecast.json", __dir__))
end
