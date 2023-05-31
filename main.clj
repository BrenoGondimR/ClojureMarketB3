(ns main
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clj-http.client :as http]))

(defn csv->maps [filename]
  (with-open [reader (io/reader filename :encoding "UTF-8")]
    (let [data (csv/read-csv reader)
          headers (map keyword (first data))
          rows (rest data)]
      (doall (map (fn [row] (zipmap headers row)) rows)))))

(defn select-keys-from-maps [maps keys]
  (map #(select-keys % keys) maps))

(defn format-to-json-string [data]
  (let [json-object (json/write-str data :escape false :pretty true)]
    (str/replace json-object #"[{}]" "")))

(defn get-stock-details [symbol]
  (let [api-url (str "https://brapi.dev/api/quote/" symbol)]
    (-> api-url
        http/get
        :body
        json/read-str)))

(defn print-json-from-api [api-url]
  (let [response (-> api-url
                     http/get)]
    (if (= (:status response) 200)
      (do
        (println "JSON da API:")
        (let [json-data (json/read-str (:body response))]
          (println (json/write-str json-data :indent 2))))
      (println "Erro na conexão com a API."))))

(defn print-csv-data [csv-data]
  (println "Lista do CSV:")
  (doseq [item csv-data]
    (let [json-output (format-to-json-string item)]
      (println json-output))))

(defn print-api-data [stock-code]
  (println "Obtendo detalhes da ação...")
  (let [api-url (str "https://brapi.dev/api/quote/" stock-code)]
    (print-json-from-api api-url)))

(defn -main []
  (let [csv-data (csv->maps "acoes-listadas.csv")
        keys-to-select [:Código :Nome]
        selected-data (select-keys-from-maps csv-data keys-to-select)]
    (do
      (print-csv-data selected-data)

      (println "Digite o código da ação para obter os detalhes:")
      (let [stock-code (str/trim (read-line))]
        (print-api-data stock-code))))

  (println "Encerrando o programa..."))


