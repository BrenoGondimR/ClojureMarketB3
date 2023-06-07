(ns main
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clj-http.client :as http]))

(declare -main)
(declare prompt-usuario)
(declare print-carteira-user)
(declare get-close)
(def carteira (atom {}))

(defn csv->map [filename]
  (with-open [reader (io/reader filename :encoding "UTF-8")]
    (let [data (csv/read-csv reader)
          headers (map keyword (first data))
          rows (rest data)]
      (doall (map (fn [row] (zipmap headers row)) rows)))))

(defn select-keys-from-maps [maps keys]
  (map #(select-keys % keys) maps))

(defn decode-regex [s]
  (let [matcher (re-matcher #"\\u([0-9a-fA-F]{4})" s)
        buffer (StringBuilder.)]
    (loop []
      (when (.find matcher)
        (.append buffer (char (Integer/parseInt (.group matcher 1) 16)))
        (recur)))
    (str buffer)))

(defn format-to-json-string [data]
  (let [json-object (json/write-str data :pretty true)]
    (decode-regex (str/replace json-object #"[{}]" ""))))

(defn print-carteira-user [chamar-prompt?]
  (println "Sua carteira:")
  (doseq [[acao dados] @carteira]
    (println (str "Nome da ação: " acao ", Quantidade: " (:quantidade dados) ", Valor Total: " (:valor-total dados) ", Tipo Do Ativo: " (:tipo-ativo dados))))
  (if chamar-prompt?
    (prompt-usuario false)))

(defn filtrar-carteira-ativo [filtro]
  (filter (fn [[acao dados]]
            (= filtro (:tipo-ativo dados)))
          (seq @carteira)))

(defn prompt-filtrar-carteira-ativo []
  (println "Digite o filtro do tipo de ativo:")
  (let [filtro (str/trim (read-line))]
    (let [acoes-filtradas (filtrar-carteira-ativo filtro)]
      (if (empty? acoes-filtradas)
        (println "Você não possui esse tipo de ativo na sua carteira.")
        (doseq [[acao dados] acoes-filtradas]
          (println (str "Nome da ação: " acao ", Quantidade: " (:quantidade dados) ", Valor Total: " (:valor-total dados) ", Tipo Do Ativo: " (:tipo-ativo dados))))))
    (prompt-usuario false)))


(defn mostrar-csv [csv-data]
  (println "Lista do CSV:")
  (doseq [item csv-data]
    (println (str "Código: " (:Código item) ", Nome: " (:Nome item)))))

(defn comprar-acoes [short-name close asset-type]
  (println "Quantas ações você deseja comprar? ")
  (let [quantidade (Integer/parseInt (read-line))
        total (* quantidade close)]
    (println "Ação comprada com sucesso! Quantidade: " quantidade)
    (println "Total a pagar: R$" total)
    (swap! carteira assoc short-name {:quantidade quantidade :valor-total total :tipo-ativo asset-type})
    (println "Nome da ação: " short-name)
    (println "Quantidade de ações: " quantidade)
    (println "Valor Total: " total)
    (prompt-usuario false short-name close)))

(defn vender-acoes []
  (print-carteira-user false)
  (println "Qual ação você deseja vender?")
  (let [acao (str/trim (read-line))]
    (if-let [dados (get @carteira acao)]
      (do
        (println "Quantas ações você deseja vender?")
        (let [quantidade (Integer/parseInt (read-line))
              quantidade-acao (:quantidade dados)
              valor-total (:valor-total dados)
              tipo-ativo (:tipo-ativo dados)
              close (get-close acao)  ;; Obten o valor da açao fazendo a requisicao para a api da brap puxando o valor dela
              total (* quantidade close)]
          (if (<= quantidade quantidade-acao)
            (do
              (let [novo-valor-total (- valor-total total)]
                (swap! carteira assoc acao {:quantidade (- quantidade-acao quantidade) :valor-total novo-valor-total :tipo-ativo tipo-ativo})  ;; Subtrai a quantidade vendida e troca a "struct" dela por essa nova atualzadad
                (println "Ação vendida com sucesso! Quantidade: " quantidade)
                (println "Total ganho: R$" total)
                (println "Quantidade de ações restantes: " (- quantidade-acao quantidade))
                (println "Valor Total: " novo-valor-total)
                (prompt-usuario false))
            (println "Você não tem ações suficientes para vender.")))
          (println "Você não possui essa ação em sua carteira."))))))


(defn rever-b3 []
  (let [csv-data (csv->map "acoes-listadas.csv")
        keys-to-select [:Código :Nome]
        selected-data (select-keys-from-maps csv-data keys-to-select)]
    (mostrar-csv selected-data)
    (prompt-usuario false)))


(defn get-close [stock-code]
  (let [api-url (str "https://brapi.dev/api/quote/" stock-code)
        response (http/get api-url)
        body (json/read-str (:body response) :key-fn keyword)]
    (if (= (:status response) 200)
      (let [results (get body :results)
            parsed-body (if (not-empty results)
                          (first results)
                          (throw (Exception. "Nenhum resultado encontrado para a ação.")))]
        (if parsed-body
          (get parsed-body :regularMarketPreviousClose)
          (throw (Exception. "Nenhum resultado encontrado para a ação."))))
      (throw (Exception. "Erro na conexão com a API.")))))  ;; Lançamento de exceção



(defn print-api-data []
  (println "Digite o código da ação para obter os detalhes:")
  (let [stock-code (str/trim (read-line))]
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
              (let [short-name (get parsed-body :shortName "Este campo não foi encontrado nos dados da API")
                    long-name (get parsed-body :longName "Este campo não foi encontrado nos dados da API")
                    symbol (get parsed-body :symbol "Este campo não foi encontrado nos dados da API")
                    asset-type (or (re-find #"ON" short-name) (re-find #"PN" short-name) "Não disponível")
                    variation (str (get parsed-body :regularMarketChange "Este campo não foi encontrado nos dados da API") " (" (get parsed-body :regularMarketChangePercent "Este campo não foi encontrado nos dados da API") "%)")
                    last-price (get parsed-body :regularMarketPrice "Este campo não foi encontrado nos dados da API")
                    high (get parsed-body :regularMarketDayHigh "Este campo não foi encontrado nos dados da API")
                    low (get parsed-body :regularMarketDayLow "Este campo não foi encontrado nos dados da API")
                    open (get parsed-body :regularMarketOpen "Este campo não foi encontrado nos dados da API")
                    close (get parsed-body :regularMarketPreviousClose "Este campo não foi encontrado nos dados da API")
                    time (get parsed-body :regularMarketTime "Este campo não foi encontrado nos dados da API")]
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
                (println "Hora: " time)
                (println)
                (prompt-usuario true symbol close asset-type)))
            (println "Nenhum resultado encontrado para a ação.")))
        (println "Erro na conexão com a API.")))))



(defn prompt-usuario [api-data-mode & [short-name close asset-type]]
  (println "Escolha uma opção:")
  (if api-data-mode
    (do
      (println "1 - Comprar Ações")
      (println "2 - Vender Ações")
      (println "3 - Buscar Ação")
      (println "4 - Rever B3")
      (println "5 - Ver Carteira")
      (println "6 - Filtrar Ação Por Ativo")
      (println "7 - Sair"))
    (do
      (println "1 - Vender Ações")
      (println "2 - Buscar Ação")
      (println "3 - Rever B3")
      (println "4 - Ver Carteira")
      (println "5 - Filtrar Ação Por Ativo")
      (println "6 - Sair")))
  (let [opcao (Integer/parseInt (read-line))]
    (cond
      (and (= opcao 1) api-data-mode) (comprar-acoes short-name close asset-type)
      (and (= opcao 1) (not api-data-mode)) (vender-acoes)
      (and (= opcao 2) api-data-mode) (vender-acoes)
      (and (= opcao 2) (not api-data-mode)) (print-api-data)
      (and (= opcao 3) api-data-mode) (print-api-data)
      (and (= opcao 3) (not api-data-mode)) (rever-b3)
      (and (= opcao 4) api-data-mode) (rever-b3)
      (and (= opcao 4) (not api-data-mode)) (print-carteira-user true)
      (and (= opcao 5) api-data-mode) (print-carteira-user true)
      (and (= opcao 5) (not api-data-mode)) (prompt-filtrar-carteira-ativo)
      (and (= opcao 6) api-data-mode) (prompt-filtrar-carteira-ativo)
      (and (= opcao 6) (not api-data-mode)) (println "Encerrando o programa...")
      (and (= opcao 7) api-data-mode) (println "Encerrando o programa...")
      :else (do (println "Opção inválida, tente novamente.") (if api-data-mode (prompt-usuario true short-name close) (-main))))))  ;; Chama a função main novamente se a opção for inválida

(defn -main []
  (let [csv-data (csv->map "acoes-listadas.csv")
        keys-to-select [:Código :Nome]
        selected-data (select-keys-from-maps csv-data keys-to-select)]
    (mostrar-csv selected-data)
    (print-api-data)))


