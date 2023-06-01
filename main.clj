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
  (let [api-url (str "https://brapi.dev/api/quote/" stock-code)
        response (http/get api-url)
        body (json/read-str (:body response) :key-fn keyword)]
    (if (= (:status response) 200)
      (let [results (get body :results)
            parsed-body (if (not-empty results) (first results))]
        (if parsed-body
          (do
            (println "Detalhes da ação:")
            (let [short-name (get parsed-body :shortName)
                  long-name (get parsed-body :longName)
                  symbol (get parsed-body :symbol)
                  asset-type (or (re-find #"ON" short-name) (re-find #"PN" short-name) "Não disponível")
                  variation (str (get parsed-body :regularMarketChange) " (" (get parsed-body :regularMarketChangePercent) "%)")
                  last-price (get parsed-body :regularMarketPrice)
                  high (get parsed-body :regularMarketDayHigh)
                  low (get parsed-body :regularMarketDayLow)
                  open (get parsed-body :regularMarketOpen)
                  close (get parsed-body :regularMarketPreviousClose)
                  time (get parsed-body :regularMarketTime)]
              (println "Nome da ação: " (if short-name (first (str/split short-name #" ")) "") " (" long-name ")")
              (println "Código da ação: " symbol)
              (println "Tipo de ativo: " asset-type)
              (println "Descrição da ação: Não há um campo específico para a descrição da ação neste JSON.")
              (println "Variação do dia (em R$): " (get parsed-body :regularMarketChange))
              (println "Variação do dia (percentual): " (get parsed-body :regularMarketChangePercent) "%")
              (println "Último preço: " last-price)
              (println "Preço máximo: " high)
              (println "Preço mínimo: " low)
              (println "Preço de abertura: " open)
              (println "Preço de fechamento: " close)
              (println "Hora: " time))))
        (println "Nenhum resultado encontrado para a ação.")))
    (println "Erro na conexão com a API.")))

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


