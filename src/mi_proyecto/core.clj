(ns mi-proyecto.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

;; Desarrollo de la estructura para el recetario en donde se cargan los archivos se procesan las conversiones de unidades y se generan dichos cambios

(def recipe-filenames
  ["recetas/Pan-Seared Steak with Garlic Butter.txt"
   "recetas/Fettuccine Alfredo.txt"
   "recetas/Lemon Cake-1.txt"
   "recetas/Best Homemade Brownies-1.txt"
   "recetas/Chimichurri Sauce.txt"])

(defn load-recipe-content
  [filename]
  (try
    (slurp (io/file filename))
    (catch Exception e
      (println (str "Error loading file " filename ": " (.getMessage e)))
      nil)))


(def ingredient-conversion-ratios
  {"all-purpose flour"      {:cup-to-gram 120 :calories-per-gram 3.64}
   "almond flour"           {:cup-to-gram 96 :calories-per-gram 5.76}
   "granulated sugar"       {:cup-to-gram 200 :calories-per-gram 3.87}
   "powdered sugar"         {:cup-to-gram 120 :calories-per-gram 3.89}
   "cocoa powder"           {:cup-to-gram 100 :calories-per-gram 2.28}
   "dark chocolate chips"   {:cup-to-gram 170 :calories-per-gram 5.3}
   "eggs"                   {:unit-to-gram 50 :cup-to-gram 240 :calories-per-gram 1.55}
   "vegetable oil"          {:cup-to-gram 216 :calories-per-gram 9}
   "canola oil"             {:cup-to-gram 216 :calories-per-gram 9}
   "extra-virgin olive oil" {:cup-to-gram 216 :calories-per-gram 9}
   "unsalted butter"        {:cup-to-gram 227 :calories-per-gram 7.17}
   "heavy cream"            {:cup-to-gram 238 :calories-per-gram 2.05}
   "fettuccine pasta"       {:unit-to-gram 28.35 :cup-to-gram 100 :calories-per-gram 3.79}
   "romano cheese"          {:cup-to-gram 100 :unit-to-gram 100 :calories-per-gram 3.96}
   "parmesan cheese"        {:cup-to-gram 100 :unit-to-gram 100 :calories-per-gram 4.3}
   "sea salt"               {:unit-to-gram 1 :cup-to-gram 290 :calories-per-gram 0}
   "black pepper"           {:unit-to-gram 1 :cup-to-gram 120 :calories-per-gram 0}
   "baking powder"          {:unit-to-gram 1 :cup-to-gram 192 :calories-per-gram 0}
   "kosher salt"            {:unit-to-gram 1 :cup-to-gram 220 :calories-per-gram 0}
   "vanilla extract"        {:unit-to-gram 1 :cup-to-gram 236.588 :calories-per-gram 0}
   "lemon zest"             {:unit-to-gram 1 :cup-to-gram 96 :calories-per-gram 0}
   "fresh lemon juice"      {:cup-to-gram 244 :calories-per-gram 0.22}
   "water"                  {:cup-to-gram 236 :calories-per-gram 0}
   "garlic"                 {:unit-to-gram 5 :cup-to-gram 160 :calories-per-gram 1.49}
   "rosemary"               {:unit-to-gram 1 :cup-to-gram 25 :calories-per-gram 0}
   "white wine vinegar"     {:cup-to-gram 240 :calories-per-gram 0.18}
   "garlic salt"            {:unit-to-gram 1 :cup-to-gram 240 :calories-per-gram 0}
   "dried oregano"          {:unit-to-gram 1 :cup-to-gram 48 :calories-per-gram 0}
   "red pepper flakes"      {:unit-to-gram 1 :cup-to-gram 90 :calories-per-gram 0}
   "smoked paprika"         {:unit-to-gram 1 :cup-to-gram 120 :calories-per-gram 0}
   "fresh flat-leaf parsley" {:cup-to-gram 30 :calories-per-gram 0.36}
   "new york strip steaks"  {:unit-to-gram 453.6 :cup-to-gram 140 :calories-per-gram 2.71}
   "ribeye"                 {:unit-to-gram 453.6 :cup-to-gram 140 :calories-per-gram 2.71}
   "top sirloin steaks"     {:unit-to-gram 453.6 :cup-to-gram 140 :calories-per-gram 2.71}
   "butter"                 {:cup-to-gram 227 :calories-per-gram 7.17}
   "salt"                   {:unit-to-gram 1 :cup-to-gram 290 :calories-per-gram 0}
   "pepper"                 {:unit-to-gram 1 :cup-to-gram 120 :calories-per-gram 0}
   "oil"                    {:cup-to-gram 216 :calories-per-gram 9}})

(def unit-aliases
  {"cups" "cup"
   "teaspoons" "tsp"
   "teaspoon" "tsp"
   "tablespoons" "tbsp"
   "tablespoon" "tbsp"
   "lbs" "lb"
   "ounces" "ounce"
   "cloves" "clove"
   "sprigs" "sprig"
   "large" "unit"
   "dashes" "dash"
   "grams" "gram"
   "eggs" "egg"})

(def common-unit-conversions
  {"cup"   236.588
   "tbsp"  14.7868
   "tsp"   4.9289
   "lb"    453.592
   "ounce" 28.3495
   "pint"  473.176
   "dash"  0.6
   "gram"  1.0})

(defn normalize-unit
  [unit]
  (get unit-aliases (str/lower-case unit) (str/lower-case unit)))

(defn convert-to-base-unit
  [value unit ingredient-name]
  (let [normalized-unit (normalize-unit unit)
        ratios (get ingredient-conversion-ratios (str/lower-case ingredient-name))
        cup-to-gram (:cup-to-gram ratios)
        unit-to-gram-ingredient-specific (:unit-to-gram ratios)]
    (cond
      (and unit-to-gram-ingredient-specific
           (contains? #{"clove" "sprig" "unit" "egg"} normalized-unit))
      (* (double value) unit-to-gram-ingredient-specific)

      (and (= normalized-unit "cup") cup-to-gram)
      (* (double value) cup-to-gram)

      (get common-unit-conversions normalized-unit)
      (* (double value) (get common-unit-conversions normalized-unit))

      (= normalized-unit "gram") (double value)

      :else
      nil)))

(defn convert-units
  [value unit system ingredient-name]
  (let [base-grams (convert-to-base-unit value unit ingredient-name)
        ratios (get ingredient-conversion-ratios (str/lower-case ingredient-name))]
    (cond
      (nil? base-grams) (str value " " unit)

      (= system :metric)
      (format "%.2f grams" (double base-grams))

      (= system :cup)
      (let [cup-to-gram (:cup-to-gram ratios)]
        (cond
          cup-to-gram (format "%.2f cups" (/ (double base-grams) cup-to-gram))
          :else (format "%.2f grams" (double base-grams))))
      :else (str value " " unit))))

(defn convert-temperature
  [temp-val from-unit to-unit]
  (cond
    (and (= from-unit :F) (= to-unit :C))
    (* (- (double temp-val) 32) 5/9)
    (and (= from-unit :C) (= to-unit :F))
    (+ (* (double temp-val) 9/5) 32)
    :else (double temp-val)))

;; Basado en los datos numericos establecidos anteriormente se hace un calculo para obtener las calorias de cada porcion total

(defn calculate-calories-for-ingredient
  [quantity unit ingredient-name]
  (let [ratios (get ingredient-conversion-ratios (str/lower-case ingredient-name))
        calories-per-gram (:calories-per-gram ratios)]
    (if calories-per-gram
      (let [grams (convert-to-base-unit quantity unit ingredient-name)]
        (if grams
          (* (double grams) (double calories-per-gram))
          0.0))
      0.0)))

;; Convierte datos string a numéricos, incluyendo fracciones y números mixtos

(defn parse-number
  [s-input]
  (let [s (str s-input)]
    (when (and s (string? s) (seq s))
      (cond
        (re-matches #"^\s*\d+\s*/\s*\d+\s*$" s)
        (let [[num-str den-str] (str/split s #"/")]
          (/ (double (Double/parseDouble (str/trim num-str)))
             (Double/parseDouble (str/trim den-str))))

        (re-matches #"^\s*(\d+)\s+(\d+/\d+)\s*$" s)
        (let [[_ whole-str frac-str] (re-find #"^\s*(\d+)\s+(\d+/\d+)\s*$" s)
              [num-str den-str] (str/split frac-str #"/")]
          (+ (Double/parseDouble whole-str)
             (/ (double (Double/parseDouble num-str)) (Double/parseDouble den-str))))

        (re-matches #"^\s*\d+(\.\d+)?\s*$" s)
        (Double/parseDouble (str/trim s))

        :else
        nil))))

;; Tomando en cuenta las palabras claves las busca detalladamente en cada receta y generalizando textos

(defn filter-recipes
  [recipes filter-keyword]
  (if (= (str/lower-case filter-keyword) "all")
    recipes
    (into {} (filter (fn [[_ recipe-content]]
                       (str/includes? (str/lower-case recipe-content) (str/lower-case filter-keyword)))
                     recipes))))

;; Para el estandar establecido en el archivo options.txt, se parsean las opciones y se convierten a tipos adecuados

(defn parse-options

  [options-content]
  (let [lines (str/split-lines options-content)
        options-map (reduce
                     (fn [acc line]
                       (if-let [[_ key val] (re-find #"^\s*(\w+):\s*(.*)$" line)]
                         (assoc acc (keyword (str/lower-case key)) (str/trim val))
                         acc))
                     {}
                     lines)
        porciones (try (if-let [p (:porciones options-map)]
                         (Integer/parseInt p)
                         1)
                       (catch Exception _ (println "Warning: Invalid 'porciones' in options.txt, defaulting to 1.") 1))
        temp (if-let [t (:temp options-map)]
               (let [k (keyword (str/upper-case t))]
                 (if (#{:F :C} k) k (do (println "Warning: Invalid 'temp' in options.txt, defaulting to :F.") :F)))
               :F)
        sistema (if-let [s (:sistema options-map)]
                  (let [k (keyword (str/lower-case s))]
                    (if (#{:metric :cup} k) k (do (println "Warning: Invalid 'sistema' in options.txt, defaulting to :metric.") :metric)))
                  :metric)
        filtra (if-let [f (:filtra options-map)]
                 (str/lower-case f)
                 "all")]
    {:porciones porciones
     :temp temp
     :sistema sistema
     :filtra filtra}))

;; Los ingredientes son limpiados y normalizados para evitar errores de formato y asegurar consistencia

(def common-ingredient-cleanups
  #",\s*(sifted|freshly ground|peeled and quartered|for dusting|coarse stems removed|or any high heat cooking oil like canola or extra light olive oil|or ribeye or top sirloin steaks|to taste|large|minced|clove|cloves|chopped)\b")

(defn clean-ingredient-name
  [raw-name-part]
  (-> (str/lower-case raw-name-part)
      (str/replace #"\*\s*$" "")
      (str/replace common-ingredient-cleanups "")
      (str/replace #"\s*\([^)]*\)" "")
      (str/replace #"\bde\s+" "")
      (str/replace #"\bfresh\s+" "")
      (str/replace #"\s*,\s*" "")
      (str/replace #"\b(large|minced|clove|cloves|sprig|sprigs|chopped|dry|grated|finely)\b" "")
      (str/trim)))

(def section-header-patterns
  {"prep time" #"(?i)Prep Time:?"
   "cook time" #"(?i)Cook Time:?"
   "total time" #"(?i)Total Time:?"
   "category" #"(?i)Category:?"
   "servings" #"(?i)Servings:?"
   "yield" #"(?i)Yield:?"
   "ingredients" #"(?i)Ingredients:?"
   "instructions" #"(?i)Instructions:?"})

(defn get-section-content
  [lines section-keyword]
  (let [current-header-pattern (get section-header-patterns section-keyword)
        start-line-idx (first (keep-indexed #(if (re-find current-header-pattern %2) %1) lines))
        relevant-lines-start-idx (if start-line-idx (inc start-line-idx) 0)
        remaining-lines (subvec (vec lines) relevant-lines-start-idx)
        next-header-pattern (re-pattern (str "(?im)^\\s*(?:"
                                             (str/join "|" (map (fn [kw] (java.util.regex.Pattern/quote kw))
                                                                (remove (fn [k] (= k section-keyword)) (keys section-header-patterns))))
                                             "):?"))
        end-line-idx (first (keep-indexed #(if (re-find next-header-pattern %2) %1) remaining-lines))]

    (if (nil? start-line-idx)
      nil
      (if end-line-idx
        (str/join "\n" (subvec remaining-lines 0 end-line-idx))
        (str/join "\n" remaining-lines)))))


(defn parse-recipe
  [recipe-content-input]
  (let [normalized-content (-> (str recipe-content-input)
                               (str/replace #"\u00A0" " ")
                               (str/replace #"\r\n|\r" "\n")
                               (str/replace #"[ \t]+" " ")
                               (str/trim))
        lines (str/split-lines normalized-content)
        first-line-candidate (first (filter #(not (str/blank? %)) lines))
        title (if (and first-line-candidate (not (str/blank? first-line-candidate)))
                (str/trim first-line-candidate)
                "Untitled Recipe")
        ingredients-raw (get-section-content lines "ingredients")
        instructions-raw (get-section-content lines "instructions")
        prep-time-line (first (filter #(re-find #"(?i)Prep Time:?" %) lines))
        cook-time-line (first (filter #(re-find #"(?i)Cook Time:?" %) lines))
        total-time-line (first (filter #(re-find #"(?i)Total Time:?" %) lines))
        category-line (first (filter #(re-find #"(?i)Category:?" %) lines))
        servings-line (first (filter #(re-find #"(?i)(?:Servings|Serves|Yield)[\s:-]+" %) lines))
        prep-time (if-let [match (re-find #"\d+" (or prep-time-line ""))] (Integer/parseInt match) 0)
        cook-time (if-let [match (re-find #"\d+" (or cook-time-line ""))] (Integer/parseInt match) 0)
        total-time (if-let [match (re-find #"\d+" (or total-time-line ""))] (Integer/parseInt match) 0)
        category (if-let [match (re-find #"(?i)Category:?\s*(.+)" (or category-line ""))] (str/trim (nth match 1)) "N/A")
        servings (if-let [s-val servings-line]
                   (if-let [num (re-find #"\d+" s-val)]
                     (Integer/parseInt num)
                     1)
                   1)]
    {:title title
     :original-content recipe-content-input
     :servings servings
     :prep-time prep-time
     :cook-time cook-time
     :total-time total-time
     :category category
     :ingredients-raw (or ingredients-raw "")
     :instructions-raw (or instructions-raw "")}))

(defn parse-ingredients
  [ingredients-raw]
  (let [lines (str/split-lines ingredients-raw)
        all-units-list (sort-by count > (distinct (concat (keys unit-aliases) (vals unit-aliases) (keys common-unit-conversions)
                                                          ["egg" "eggs" "clove" "cloves" "sprig" "sprigs" "large" "pinch" "dash"])))
        unit-patterns-regex (str/join "|" (map #(java.util.regex.Pattern/quote %) all-units-list))
        ingredient-pattern (re-pattern
                            (str "^\\s*[-*]?\\s*"
                                 "([\\d\\s/]+(?:\\.\\d+)?)"
                                 "\\s*"
                                 "(?:"
                                 "(?i:(" unit-patterns-regex "))\\b\\s*"
                                 ")?"
                                 "(.*)"))]
    (keep
     (fn [line]
       (if-let [match (re-find ingredient-pattern (str/trim line))]
         (let [[_ quantity-str unit-str-raw name-part-raw] match
               quantity (parse-number (str/trim quantity-str))
               raw-unit (if (str/blank? unit-str-raw) "unit" (normalize-unit unit-str-raw))
               name-to-clean (if (and (not (str/blank? unit-str-raw)) (str/blank? name-part-raw))
                               unit-str-raw
                               name-part-raw)
               cleaned-name (let [name-part (str/trim name-to-clean)
                                  final-name (cond
                                               (re-find #"(?i)\beggs?\b" name-part) "eggs"
                                               (str/includes? (str/lower-case name-part) "new york strip steaks") "new york strip steaks"
                                               (str/includes? (str/lower-case name-part) "fettuccine pasta") "fettuccine pasta"
                                               (str/includes? (str/lower-case name-part) "garlic") "garlic"
                                               (str/includes? (str/lower-case name-part) "all-purpose flour") "all-purpose flour"
                                               (str/includes? (str/lower-case name-part) "almond flour") "almond flour"
                                               (str/includes? (str/lower-case name-part) "granulated sugar") "granulated sugar"
                                               (str/includes? (str/lower-case name-part) "powdered sugar") "powdered sugar"
                                               (str/includes? (str/lower-case name-part) "cocoa powder") "cocoa powder"
                                               (str/includes? (str/lower-case name-part) "dark chocolate chips") "dark chocolate chips"
                                               (str/includes? (str/lower-case name-part) "canola oil") "canola oil"
                                               (str/includes? (str/lower-case name-part) "vegetable oil") "vegetable oil"
                                               (str/includes? (str/lower-case name-part) "extra-virgin olive oil") "extra-virgin olive oil"
                                               (str/includes? (str/lower-case name-part) "unsalted butter") "unsalted butter"
                                               (str/includes? (str/lower-case name-part) "butter") "butter"
                                               (str/includes? (str/lower-case name-part) "heavy cream") "heavy cream"
                                               (str/includes? (str/lower-case name-part) "romano cheese") "romano cheese"
                                               (str/includes? (str/lower-case name-part) "parmesan cheese") "parmesan cheese"
                                               (str/includes? (str/lower-case name-part) "sea salt") "sea salt"
                                               (str/includes? (str/lower-case name-part) "kosher salt") "kosher salt"
                                               (str/includes? (str/lower-case name-part) "salt") "salt"
                                               (str/includes? (str/lower-case name-part) "black pepper") "black pepper"
                                               (str/includes? (str/lower-case name-part) "pepper") "pepper"
                                               (str/includes? (str/lower-case name-part) "baking powder") "baking powder"
                                               (str/includes? (str/lower-case name-part) "vanilla extract") "vanilla extract"
                                               (re-find #"(?i)lemon zest|zest of.*lemon" name-part) "lemon zest"
                                               (str/includes? (str/lower-case name-part) "fresh lemon juice") "fresh lemon juice"
                                               (str/includes? (str/lower-case name-part) "water") "water"
                                               (str/includes? (str/lower-case name-part) "rosemary") "rosemary"
                                               (str/includes? (str/lower-case name-part) "white wine vinegar") "white wine vinegar"
                                               (str/includes? (str/lower-case name-part) "garlic salt") "garlic salt"
                                               (str/includes? (str/lower-case name-part) "dried oregano") "dried oregano"
                                               (str/includes? (str/lower-case name-part) "red pepper flakes") "red pepper flakes"
                                               (str/includes? (str/lower-case name-part) "smoked paprika") "smoked paprika"
                                               (str/includes? (str/lower-case name-part) "fresh flat-leaf parsley") "fresh flat-leaf parsley"
                                               :else (clean-ingredient-name name-part))]
                              final-name)]
           (when (and quantity (not (str/blank? cleaned-name)))
             {:quantity quantity
              :unit raw-unit
              :name cleaned-name
              :original-line line}))
         (when-let [match (re-find #"^(\D+)$" (str/trim line))]
           (let [name-part (first match)
                 cleaned-name (cond
                                (str/includes? (str/lower-case name-part) "powdered sugar") "powdered sugar"
                                :else (clean-ingredient-name name-part))]
             (when (not (str/blank? cleaned-name))
               {:quantity 0.0
                :unit "unit"
                :name cleaned-name
                :original-line line})))))
     lines)))

(defn process-recipe
  [recipe-data options]
  (let [{:keys [title original-content servings prep-time cook-time total-time category
                ingredients-raw instructions-raw]} recipe-data
        target-servings (:porciones options)
        temp-unit-target (:temp options)
        measurement-system-target (:sistema options)
        scaled-servings (if (and target-servings (not= target-servings servings))
                          target-servings
                          servings)
        parsed-ingredients-result (parse-ingredients ingredients-raw)
        scaled-ingredients (map (fn [ing]
                                  (if (and target-servings (not= target-servings servings))
                                    (update ing :quantity #(* (double %) (/ (double target-servings) (double servings))))
                                    ing))
                                parsed-ingredients-result)
        processed-ingredients (map (fn [ing]
                                     (assoc ing
                                            :display-quantity (convert-units (:quantity ing) (:unit ing) measurement-system-target (:name ing))
                                            :calories (calculate-calories-for-ingredient (:quantity ing) (:unit ing) (:name ing))))
                                   scaled-ingredients)
        total-recipe-calories-sum (reduce + (map :calories processed-ingredients))
        calories-per-serving (if (pos? scaled-servings) (/ (double total-recipe-calories-sum) (double scaled-servings)) 0.0)
        processed-instructions (str/replace instructions-raw
                                            #"(\d+)\s*°?([CF])"
                                            (fn [[_ temp-val-str unit-str]]
                                              (let [temp-val (Double/parseDouble temp-val-str)
                                                    original-unit (keyword (str/upper-case unit-str))]
                                                (if (not= original-unit temp-unit-target)
                                                  (format "%.0f°%s" (convert-temperature (double temp-val) original-unit temp-unit-target) (name temp-unit-target))
                                                  (str temp-val-str "°" unit-str)))))]
    {:title title
     :original-content original-content
     :processed-servings scaled-servings
     :prep-time prep-time
     :cook-time cook-time
     :total-time total-time
     :category category
     :processed-ingredients processed-ingredients
     :processed-instructions processed-instructions
     :total-calories (double total-recipe-calories-sum)
     :calories-per-serving calories-per-serving
     :options options}))

;; Procesamiento en paralelo el cual permite que las recetas se procesen de manera masiva y eficiente en paralelo.

(defn process-all-recipes-sequentially
  [recipes-map options]
  (doall (map (fn [[_ content]]
                (when content
                  (let [recipe-data (parse-recipe content)
                        processed (process-recipe recipe-data options)]
                    processed)))
              recipes-map)))

(defn process-all-recipes-parallel
  [recipes-map options]
  (doall (pmap (fn [[_ content]]
                 (when content
                   (let [recipe-data (parse-recipe content)
                         processed (process-recipe recipe-data options)]
                     processed)))
               recipes-map)))

(defn escape-html [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&#x27;")))

(defn generate-html-report
  [recipe-map seq-ms par-ms]
  (let [title (:title recipe-map)
        base-name (-> title (str/replace #"[^a-zA-Z0-9]+" "_") (str ".html"))
        filename (str "reportes/" base-name)
        servings (:processed-servings recipe-map)
        calories (format "%.2f" (:total-calories recipe-map))
        cal-per-serving (format "%.2f" (:calories-per-serving recipe-map))
        ingredientes (:processed-ingredients recipe-map)
        instrucciones (:processed-instructions recipe-map)
        original (escape-html (:original-content recipe-map))
        speedup (/ seq-ms par-ms)
        max-threads 10
        efficiencies (for [threads (range 1 (inc max-threads))]
                       {:threads threads
                        :eff (format "%.2f" (/ speedup threads))})
        css-style "
        <style>
        body { font-family: 'Segoe UI', sans-serif; background: #fffdf5; color: #333; margin: 2em; line-height: 1.6; }
        h1 { color: #8B0000; }
        h2 { color: #006400; }
        .metric { color: #1e90ff; font-weight: bold; }
        .ingredient { color: #8b4513; }
        .step { color: #2f4f4f; margin-left: 1em; }
        .temperature { color: #ff4500; font-weight: bold; }
        .highlight { background-color: #ffffcc; }
        .block { background-color: #f9f9f9; border-left: 5px solid #ccc; padding: 1em; margin: 1em 0; }
        </style>"]
    (spit filename
          (str "<html><head><meta charset='UTF-8'><title>" (escape-html title) "</title>"
               css-style
               "</head><body>"
               "<h1>" (escape-html title) "</h1>"
               "<div class='block'><strong>Portions:</strong> <span class='metric'>" servings "</span><br/>"
               "<strong>Total Calories:</strong> <span class='metric'>" calories " kcal</span><br/>"
               "<strong>Calories per portion:</strong> <span class='metric'>" cal-per-serving " kcal</span><br/>"
               "<strong>Speedup:</strong> " (format "%.2f" speedup) "<br/>"
               "<strong>Efficiency:</strong><ul>"
               (apply str (for [{:keys [threads eff]} efficiencies]
                            (str "<li>" threads " hilos: " eff "</li>"))) "</ul>"
               "</div>"

               "<h2>Ingredients</h2><ul>"
               (apply str (for [i ingredientes]
                            (str "<li class='ingredient'><span class='metric'>" (:display-quantity i) "</span> "
                                 (escape-html (:name i)) " "
                                 "(<span class='metric'>" (format "%.2f" (:calories i)) " kcal</span>)</li>")))
               "</ul>"

               "<h2>Instructions</h2>"
               (let [step-lines (filter (complement str/blank?) (str/split-lines instrucciones))]
                 (apply str
                        (map-indexed
                         (fn [_ line]
                           (let [step-label (if (re-find #"^\\s*\\d+[\\.|\\)]" line)
                                              ""
                                              (str "<strong> Paso "  "</strong> "))]
                             (str "<p class='step'>" step-label (escape-html line) "</p>")))
                         step-lines)))

               "<h2>Contenido Original</h2><pre class='highlight'>" original "</pre>"
               "</body></html>"))))

;; Pra ejecutarlo en la terminal se define el main el cual contiene de manera lineal el flujo del programa, desde la carga de recetas hasta el procesamiento y generación de reportes HTML.

(defn -main
  [& _]
  (let [chosen-options-content (try (slurp (io/file "recetas/options.txt"))
                                    (catch Exception _
                                      (println "Error: No se encontró 'recetas/options.txt'")
                                      (System/exit 1)))
        options (parse-options chosen-options-content)
        filter-keyword (:filtra options)
        loaded-recipes (into {} (keep (fn [filename]
                                        (if-let [content (load-recipe-content filename)]
                                          [(str/replace filename #"\.txt$" "") content]))
                                      recipe-filenames))
        recipes-to-process loaded-recipes
        filtered-recipes (filter-recipes recipes-to-process filter-keyword)
        start-time-seq (System/nanoTime)
        processed-results-sequential (process-all-recipes-sequentially filtered-recipes options)
        end-time-seq (System/nanoTime)
        sequential-time-ms (/ (- end-time-seq start-time-seq) 1000000.0)
        start-time-par (System/nanoTime)
        _ (process-all-recipes-parallel filtered-recipes options)
        end-time-par (System/nanoTime)
        parallel-time-ms (/ (- end-time-par start-time-par) 1000000.0)]

    (println "\n--- Iniciando Procesamiento ---")
    (println (str "Procesamiento secuencial completado en " (format "%.2f" sequential-time-ms) " ms."))
    (println (str "Procesamiento paralelo completado en " (format "%.2f" parallel-time-ms) " ms."))

    (doseq [recipe-map (remove nil? processed-results-sequential)]
      (generate-html-report recipe-map sequential-time-ms parallel-time-ms)
      (println (str "Archivo HTML generado para: " (:title recipe-map))))

    (System/exit 0)))