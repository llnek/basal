;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; Copyright (c) 2013-2016, Kenneth Leung. All rights reserved.

(ns ^{:doc "Country-codes and country-names."
      :author "Kenneth Leung" }

  czlab.xlib.countries

  (:require [czlab.xlib.logging :as log]
            [clojure.string :as cs])

  (:use [czlab.xlib.str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _ccodes {
    "AF"  "Afghanistan"
    "AL"  "Albania"
    "DZ"  "Algeria"
    "AS"  "American Samoa"
    "AD"  "Andorra"
    "AO"  "Angola"
    "AI"  "Anguilla"
    "AQ"  "Antarctica"
    "AG"  "Antigua and Barbuda"
    "AR"  "Argentina"
    "AM"  "Armenia"
    "AW"  "Aruba"
    "AU"  "Australia"
    "AT"  "Austria"
    "AZ"  "Azerbaijan"
    "BS"  "Bahamas"
    "BH"  "Bahrain"
    "BD"  "Bangladesh"
    "BB"  "Barbados"
    "BY"  "Belarus"
    "BE"  "Belgium"
    "BZ"  "Belize"
    "BJ"  "Benin"
    "BM"  "Bermuda"
    "BT"  "Bhutan"
    "BO"  "Bolivia"
    "BA"  "Bosnia and Herzegowina"
    "BW"  "Botswana"
    "BV"  "Bouvet Island"
    "BR"  "Brazil"
    "VG"  "Virgin Islands (British)"
    "IO"  "British Indian Ocean Territory"
    "BN"  "Brunei Darussalam"
    "BG"  "Bulgaria"
    "BF"  "Burkina Faso"
    "BI"  "Burundi"
    "KH"  "Cambodia"
    "CM"  "Cameroon"
    "CA"  "Canada"
    "CV"  "Cape Verde"
    "KY"  "Cayman Islands"
    "CF"  "Central African Republic"
    "TD"  "Chad"
    "CL"  "Chile"
    "CN"  "China"
    "CX"  "Christmas Island"
    "CC"  "Cocos (Keeling) Islands"
    "CO"  "Colombia"
    "KM"  "Comoros"
    "CG"  "Congo"
    "CD"  "Congo Democratic Republic"
    "CK"  "Cook Islands"
    "CR"  "Costa Rica"
    "CI"  "Cote D'Ivoire"
    "HR"  "Croatia"
    "CY"  "Cyprus"
    "CZ"  "Czech Republic"
    "DK"  "Denmark"
    "DJ"  "Djibouti"
    "DM"  "Dominica"
    "DO"  "Dominican Republic"
    "TP"  "East Timor"
    "EC"  "Ecuador"
    "EG"  "Egypt"
    "SV"  "El Salvador"
    "GQ"  "Equatorial Guinea"
    "ER"  "Eritrea"
    "EE"  "Estonia"
    "ET"  "Ethiopia"
    "FK"  "Falkland Islands (Malvinas)"
    "FO"  "Faroe Islands"
    "FJ"  "Fiji"
    "FI"  "Finland"
    "FR"  "France"
    "GF"  "French Guiana"
    "PF"  "French Polynesia"
    "TF"  "French Southern Territories"
    "GA"  "Gabon"
    "GM"  "Gambia"
    "GE"  "Georgia"
    "DE"  "Germany"
    "GH"  "Ghana"
    "GI"  "Gibraltar"
    "GR"  "Greece"
    "GL"  "Greenland"
    "GD"  "Grenada"
    "GP"  "Guadeloupe"
    "GU"  "Guam"
    "GT"  "Guatemala"
    "GN"  "Guinea"
    "GW"  "Guinea-Bissau"
    "GY"  "Guyana"
    "HT"  "Haiti"
    "HM"  "Heard and McDonald Islands"
    "HN"  "Honduras"
    "HK"  "Hong Kong"
    "HU"  "Hungary"
    "IS"  "Iceland"
    "IN"  "India"
    "ID"  "Indonesia"
    "IE"  "Ireland"
    "IL"  "Israel"
    "IT"  "Italy"
    "JM"  "Jamaica"
    "JP"  "Japan"
    "JO"  "Jordan"
    "KZ"  "Kazakhstan"
    "KE"  "Kenya"
    "KI"  "Kiribati"
    "KR"  "Korea - Republic of"
    "KW"  "Kuwait"
    "KG"  "Kyrgyzstan"
    "LA"  "Lao People's Democratic Republic"
    "LV"  "Latvia"
    "LB"  "Lebanon"
    "LS"  "Lesotho"
    "LR"  "Liberia"
    "LI"  "Liechtenstein"
    "LT"  "Lithuania"
    "LU"  "Luxembourg"
    "MO"  "Macau"
    "MK"  "Macedonia (former Yugoslav Rep.)"
    "MG"  "Madagascar"
    "MW"  "Malawi"
    "MY"  "Malaysia"
    "MV"  "Maldives"
    "ML"  "Mali"
    "MT"  "Malta"
    "MH"  "Marshall Islands"
    "MQ"  "Martinique"
    "MR"  "Mauritania"
    "MU"  "Mauritius"
    "YT"  "Mayotte"
    "MX"  "Mexico"
    "FM"  "Micronesia - Federated States of"
    "MD"  "Moldova - Republic of"
    "MC"  "Monaco"
    "MN"  "Mongolia"
    "MS"  "Montserrat"
    "MA"  "Morocco"
    "MZ"  "Mozambique"
    "MM"  "Myanmar"
    "NA"  "Namibia"
    "NR"  "Nauru"
    "NP"  "Nepal"
    "NL"  "Netherlands"
    "AN"  "Netherlands Antilles"
    "NC"  "New Caledonia"
    "NZ"  "New Zealand"
    "NI"  "Nicaragua"
    "NE"  "Niger"
    "NG"  "Nigeria"
    "NU"  "Niue"
    "NF"  "Norfolk Island"
    "MP"  "Northern Mariana Islands"
    "NO"  "Norway"
    "OM"  "Oman"
    "PK"  "Pakistan"
    "PW"  "Palau"
    "PS"  "Palestine"
    "PA"  "Panama"
    "PG"  "Papua New Guinea"
    "PY"  "Paraguay"
    "PE"  "Peru"
    "PH"  "Philippines"
    "PN"  "Pitcairn"
    "PL"  "Poland"
    "PT"  "Portugal"
    "PR"  "Puerto Rico"
    "QA"  "Qatar"
    "RE"  "Reunion"
    "RO"  "Romania"
    "RU"  "Russian Federation"
    "RW"  "Rwanda"
    "KN"  "Saint Kitts and Nevis"
    "LC"  "Saint Lucia"
    "VC"  "Saint Vincent and the Grenadines"
    "WS"  "Samoa"
    "SM"  "San Marino"
    "ST"  "Sao Tome and Principe"
    "SA"  "Saudi Arabia"
    "SN"  "Senegal"
    "CS"  "Serbia and Montenegro"
    "SC"  "Seychelles"
    "SL"  "Sierra Leone"
    "SG"  "Singapore"
    "SK"  "Slovakia (Slovak Republic)"
    "SI"  "Slovenia"
    "SB"  "Solomon Islands"
    "SO"  "Somalia"
    "ZA"  "South Africa"
    "ES"  "Spain"
    "LK"  "Sri Lanka"
    "SH"  "St. Helena"
    "PM"  "St. Pierre and Miquelon"
    "SR"  "Suriname"
    "SJ"  "Svalbard and Jan Mayen Islands"
    "SZ"  "Swaziland"
    "SE"  "Sweden"
    "CH"  "Switzerland"
    "TW"  "Taiwan"
    "TJ"  "Tajikistan"
    "TZ"  "Tanzania - United Republic of"
    "TH"  "Thailand"
    "TG"  "Togo"
    "TK"  "Tokelau"
    "TO"  "Tonga"
    "TT"  "Trinidad and Tobago"
    "TN"  "Tunisia"
    "TR"  "Turkey"
    "TM"  "Turkmenistan"
    "TC"  "Turks and Caicos Islands"
    "TV"  "Tuvalu"
    "UG"  "Uganda"
    "UA"  "Ukraine"
    "AE"  "United Arab Emirates"
    "GB"  "United Kingdom"
    "US"  "United States"
    "UM"  "United States Minor Outlying Islands"
    "UY"  "Uruguay"
    "UZ"  "Uzbekistan"
    "VU"  "Vanuatu"
    "VA"  "Vatican City State (Holy See)"
    "VE"  "Venezuela"
    "VN"  "Vietnam"
    "VI"  "Virgin Islands (U.S.)"
    "WF"  "Wallis And Futuna Islands"
    "EH"  "Western Sahara"
    "YE"  "Yemen"
    "YU"  "Yugoslavia"
    "ZM"  "Zambia"
    "ZW"  "Zimbabwe"
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _ccodes-seq (seq _ccodes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn findCountry
  "The full country name"
  ^String
  [^String code]
  (get _ccodes (ucase code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listCodes "List all the country codes" [] (keys _ccodes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isUSA? "If the code is US" [^String code] (= "US" (ucase code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn findCountryCode
  "The country code"
  ^String
  [^String country]
  (when-some [rs (filter #(= (nth % 1) country) _ccodes-seq)]
    (nth (first rs) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _states {
    "AL"  "Alabama"
    "AK"  "Alaska"
    "AZ"  "Arizona"
    "AR"  "Arkansas"
    "CA"  "California"
    "CO"  "Colorado"
    "CT"  "Connecticut"
    "DE"  "Delaware"
    "FL"  "Florida"
    "GA"  "Georgia"
    "HI"  "Hawaii"
    "ID"  "Idaho"
    "IL"  "Illinois"
    "IN"  "Indiana"
    "IA"  "Iowa"
    "KS"  "Kansas"
    "KY"  "Kentucky"
    "LA"  "Louisiana"
    "ME"  "Maine"
    "MD"  "Maryland"
    "MA"  "Massachusetts"
    "MI"  "Michigan"
    "MN"  "Minnesota"
    "MS"  "Mississippi"
    "MO"  "Missouri"
    "MT"  "Montana"
    "NE"  "Nebraska"
    "NV"  "Nevada"
    "NH"  "New Hampshire"
    "NJ"  "New Jersey"
    "NM"  "New Mexico"
    "NY"  "New York"
    "NC"  "North Carolina"
    "ND"  "North Dakota"
    "OH"  "Ohio"
    "OK"  "Oklahoma"
    "OR"  "Oregon"
    "PA"  "Pennsylvania"
    "RI"  "Rhode Island"
    "SC"  "South Carolina"
    "SD"  "South Dakota"
    "TN"  "Tennessee"
    "TX"  "Texas"
    "UT"  "Utah"
    "VT"  "Vermont"
    "VA"  "Virginia"
    "WA"  "Washington"
    "WV"  "West Virginia"
    "WI"  "Wisconsin"
    "WY"  "Wyoming"
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _states-seq (seq _states))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listStates "List all the abbreviated states" [] (keys _states))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn findState
  "The full state name"
  ^String
  [^String code]
  (get _states (ucase code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn findStateCode
  "The abbreviated state code"
  ^String
  [^String state]
  (when-some [rs (filter #(= (nth % 1) state) _states-seq) ]
    (nth (first rs) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


