# TAREA GENOMICA FUNCIONAL  28-01-21
# Se necesita la libreria igraph para hacer esta actividad
library(igraph)

# 1. A partir de las redes de la figura 1. Calcula con igraph, las siguientes 
#    propiedades:
#       a) Vecinos.
#       b) La distribucion de conectividades
#       c) El nodo mas conectado.
#       d) El diametro
#       e) La matriz de distancias y el heatmap asociado.

#Primero se deben crear las cuatro redes de la figura 1. 
a <- make_star(10, mode = "undirected") #la función make_star crea una red en forma
                                        #de estrella. Se especifica que sea no dirigida
                                        #para que la red no tenga flechas, solo conexiones.
#la red se crea con diez nodos, Se guarda en el objeto a.

b <-make_empty_graph(10, directed = F) #la función make_empty_graph genera una red
# con solo los nodos. Se especifican 10 nodos y que sea no dirigida. Se guarda en el objeto b. 
b <- add.edges(b, c(1,10, 1,9, 1,8, 1,4, 1,2, 2,10, 2,9, 2,7, 2,6, 3,5, 3,8, 3,7, 4,9, 4,7,
                    4,8, 4,6, 4,5, 5,8, 5,7, 5,6, 6,8, 7,9, 7,10, 8,9, 9,10))
#para añadir las conexiones de la red b se usa la función add.edges, donde se especifica el orden
#que se van a conectar los nodos. Se guarda en el mismo objeto b.

c <- make_ring(10) #la función make_ring crea una red en forma
#circular. Esta red ya es no dirigida, por lo que se dejan los valores default.
#la red se crea con diez nodos, Se guarda en el objeto c.

d<-barabasi.game(10,directed = FALSE)
#la función barbasi.game genera una red free-scale. La red se crea con 10 nodos y
#se especifica que sea no dirigida. Se guarda en el objeto d.

#para realizar todos los puntos del ejercicio se crea una función
#la función se guarda en el objeto pred

pred <- function(red){ #esta función tiene como argumentos de entrada una red
    
    # Lo primero que hace la función es mandar un mensaje al usuario que la red
    # tiene determinados números de nodos usando length ( V( de la red )) y 
    # crea el gráfico de la red
    print(paste("Tu red tiene : ", length(V(red)), " nodos y se ve así --> "))
    plot(red,
         vertex.color = "firebrick1", vertex.frame.color = "firebrick",
         vertex.size = 12.5, vertex.label.cex = 1, 
         vertex.label.family= "sans", vertex.label.color = "Black", 
         vertex.label.font = 2,
         edge.color = "dodgerblue1", edge.arrow.size = 0.4, main_color = "white" )
    
    
    #Una vez que se observa la arquitectura de la red manda una pregunta al usuario
    #para que el usuario elija de qué nodos quiere obtener los vecinos usando la 
    #función readline. La respuesta debe ser el número del nodo y se gurda en un objeto
    #al que se le cambia la clase a numérico
    vecinos <- readline(prompt = paste("¿De qué nodo quieres obtener sus vecinos? "))
    vecinos <- as.numeric(vecinos)
    
    #Después genera una matriz de adyacencia usando la función get.adjacency y convirtiéndolo
    #a formato matriz, y lo guarda en un objeto.
    ma <- as.matrix( get.adjacency(red) )
    colnames(ma) <- paste("Nodo", rep(1:10)) #con colnames / rownames se agregan nombres a las columnas y
    rownames(ma) <- paste("Nodo", rep(1:10)) #renglones, pegando la palabra Nodo e indicando que lo repita 
                                             #10 veces con la función rep. Además de que agrega el número a la pabra Nodo
    vec <- sum( ma[vecinos, ] ) #Usando el objeto anterior que indcaba el nodo del que se iban a obtener los vecinos, se
                                #selecciona el renglón correspondiente a ese nodo en la matriz de adyacecia. Posteriormente
                                #se suman los elementos del renglón y se guarda en un objeto
    #Después manda un mensaje al usuario indicando que el nodo que seleccionó tiene determinado número de vecinos. Esto lo
    #hace usando la suma del renglón de ese nodo en la matriz de adyacencia, ya que indica con qué nodos están conectados.  
    #En la matriz de adyacencia se coloca un 1 con los nodos con los que está conectado y un 0 con los que no, por lo que 
    #si se suman los elementos del renglón se obtiene la cantidad de nodos que están conectados a ese nodo, o la cantidad
    #de vecinos de un nodo determinado.
    print(paste("El nodo ", vecinos , " tiene ", vec, " vecinos"))
    vecvec <- as.vector( which ( ma[vecinos,] == 1) )
    #Para determinar qué nodos son los vecinos del nodo seleccionado se usa la función which, donde se especifica cuáles 
    #nodos en el renglón del nodo seleccionado de la matriz de adyacencia son eactamente igual a 1, que representa los 
    #vecinos del nodo. Ese resultado lo guarda en formato vector dentro de un objeto.
    print(paste("Los vecinos del nodo ", vecinos, " son : "))
    print(paste(vecvec))
    #Arroja otro mensaje al usuario donde indica que los vecinos del nodo seleccionado son los del objeto que previamente
    #se obtuvo, y se obtienen cuáles son esos nodos vecinos
    
    
    #Para la distribución de conectividades arroja un mensaje al usuario indicando 
    #que se representa como una gráfica
    print(paste("Esta red tiene una distribución de conectividades así --> "))
    #Par hacer la gráfica se hace un histograma del degree.distribution de la red con
    #la funcion hist y la función degree.distribution. Para la gráfica se modificaron
    #variables estéticas
    hist(degree.distribution( red ), 
         main = "Distribución de conectividades", xlab = "degree distribution", 
         ylab = "Pk", border = "dodgerblue3", col = "midnightblue")
    
    
    #Para encontrar los nodos con más conexiones se creo un objeto que contiene el número de 
    #conexiones más alto que se encontró en la red. Para eso se ordenó el degree de la red 
    #en forma decresiente y se seleccionó el primer valor, que corresponde al número máximo
    #de conexiones. 
    variable <- sort( degree(red), decreasing = T )[1]
    variable <- variable - 1 #Después al valor maximo de conexiones se le resta 1, así, el 
    #objeto que contenia el valor máximo de conexiones queda con una unidad menos y se reasigna
    #al mismo objeto
    con <- which( degree(red) > variable) #Con la función which, del degree de la red, se 
    #seleccionan aquellos nodos que tienen un degree mayor al valor de la variable reasignada
    #Esto indica cuáles nodos específicamente tienen el número más alto de conexiones, reflejado
    #como el degree más alto, porque tienen un degree mayor al de la variable, y la variable tiene
    #el valor del degree más alto menos uno, lo que deja solamente los nodos cuyo degree es igual al 
    #valor de la variable inicialmente, lo que sería el degree más alto. Los nodos los guarda en un objeto. 
    print(paste("Los nodos que tienes más conexiones son : ")) #Envía un mensaje al usuario para 
    #indicar cuáles nodos son los que tienen el degree más alto
    print(paste(con)) #Se imprime el objeto de los nodos con el degree mayor 
    print(paste("Con ", variable + 1, " conexiones")) #Envia un mensaje al usuario para indicar que 
    #cuántas conexiones tienen los nodos con más conexiones, y esto lo hace sumando un 1 a la variable
    #a la que previamente se le había restado para regresar el valor original e indicar el número
    #mas alto de conexiones
    
    
    #Manda un mensaje al usuario para indicar el diámetro de la red
    print(paste("El diámetro de la red es : ", 
                diameter(red))) #para establecer el diámetro se usa la función diameter del paquete 
                                #igraph
    print(paste("La ruta que siguió para calcular el diámetro fue : ")) #envia un mensaje al usuario para 
    #indicar la ruta que se siguió para calcular el diámetro
    print(paste(get.diameter( red )) ) #Se imprime la ruta establecida con la función get.diameter de
    #la red
    
    
    ma2 <- as.matrix(distances( red )) #Para obtener la matriz de distancias se usa la función distances
    #con la red. Esta función se usa anidada dentro de la función as.matrix para que tenga el formato matriz
    #y se guarda en un objeto
    colnames(ma2) <- paste("Nodo", rep(1:length(red))) #Con estas funciones se les agrega nombres a los renglones y columnas
    rownames(ma2) <- paste("Nodo", rep(1:length(red))) #de la matriz, usando la función paste para que pegue la palabra Nodo
                                                       #Usando la función rep se especifica que después de la palabra Nodo agregue
                                                       #un número, en orden desde el 1 hasta la longitud de la red
    print(paste("La matriz de distancias es : ")) #Envia un mensaje al usuario indicando que se estará visualizando la matriz de distancias
    View(ma2) #Con la funcion Vies se visualiza la matriz
    print(paste("El heatmap se observa así --> ")) #Envia un mensaje al usuario para indicar el gráfico de heatmap
    heatmap( ma2 ) #con la función de heatmap sobre la matriz, se genera el heatmap correspondiente
}
#Después se aplica la función para cada red creada previamente. Para ver el histograma del degree.distribution se debe de recorrer
#a la gráfica anterior del heatmap
pred(a)
pred(b)
pred(c)
pred(d)



# 2. Elabora un programa en R que utilice un ciclo for para a partir del vector 
#    v siguiente imprima los cuadrados de los numeros impares
v <-sample(100) #con la funcion sample se genera un vector aleatorio estableciendo el número máximo 
                #de elementos y se guarda en un objeto

#para encontrar el cuadrado de los números impares de un vector se genera una función que tiene como
#argumentos de entrada un vector y se guarda en el objeto odd_squares
odd_squares <- function( vec ){
    
    #para que la función vaya iterando sobre los elementos del vector se usa un ciclo for, donde se
    #establece que para cada elemento x en 1 hasta la longitud del vector realice : 
    for ( x in 1:length(vec) ){
        if ( vec[x] %% 2 != 0 ){ #Si el elemento de la posición actual del vector dividio entre dos,
                                 #el resultado es distinto de cero, entonces se trata de un número impar,
                                 #por lo que prosigue con las acciones : 
            print(paste("El cuadrado de ", vec[x], " es ", vec[x]^2)) #Imprime en la consola el mensaje de 
                                                                      #que el cuadrado del número de la posición
                                                                      #actual en el vector es ese número elevado
                                                                      #al cuadrado
            }
    }
}
#Finalmente se comprueba la función con el vector previamente creado
odd_squares(v)



# 3. Elabora un programa en R que a partir del archivo de amistades del grupo.
#       a) Cargue el archivo
#       b) Genere el vector de nombres de todos tus amigos (los tuyos)
#       c) Genere el vector de nombres de todos los que se consideren tus amigos.
#       d) Imprima el texto: “Hola amigo1”, en donde amigo1 es el nombre de
#          cada uno de tus amigos.
#       e) Calcule el promedio de los amigos de todos
#       f) Encuentra tu coeficiente de clusterizacion

#Se elaboró una función sin argumentos de entrada y se guardó en el objeto problema_3
problema_3 <- function( ){
    library(igraph) #Como primer paso de la función es cargar la libreria de igraph
    
    print(paste("Selecciona el archivo de amistades"))#Manda el mensaje al usuario que seleccione
                                                      #la red de amistades
    ra<- read.csv(file.choose()) #Se usa la función read.csv para leer el archivo csv de excel.
    #Para cargar el archivo se usa la función file.choose() para que el usurio cargue el archivo desde
    #sus documentos. Se guarda en el objeto ra
    row.names(ra) <- ra[,1] #Se asignan los nombres de los renglones del archivo con el mismo archivo,
                            #seleccionando la primer columna y todos los renglones, que es donde están
                            #los nombres
    ra <- ra [,-1] #posteriormente se elimina la columnsa y se reasigna al mismo objeto 
    ra <- as.matrix(ra) #se cambia la clase del objeto a matriz para que se convierta en una matriz de
                        #dyacencia,ya que indica con quienes se conecta, a través de si se considera 
                        #su amigo o no
    diag(ra) <- rep(0,19) #en la diagonal de la matriz, usando la función diag(); se usa la función
                          #rep para repetir el número 0 19 veces, para que abarque todos los elementos
                          #de la diagonal
    View(ra) #Se viasualiza la matriz con las modificaciones 
    
    
    #Se crea un vector con los nombres de todos y un respectivo número, que es el orden en que están en la matriz
    losujetos <- c("(1) ANA    (2) GABRIELA    (3) CARMEN    (4) PEDRO    (5) GERARDO    (6) CINDY    (7) EDOARDO    (8) JORGE    (9) ENOE    (10) YOALI    (11) JANETZY    (12) EMILIO    (13) JAVIER    (14) FRANCISCO    (15) EDITH    (16) FERNANDA    (17) ANDRES    (18) LIZBETH    (19) ALEJANDRA")
    print(paste(losujetos)) #imprime el vector con los nombres de todos
    #Usando la función readline se muestra la pregunta al usuario, de todos los nombres mostrados en pantalla, que
    #escriba el número de la persona de quien quiere saber sus amigos. La respuesta se guarda en un objeto.
    elsujeto <- readline(prompt = paste("¿De quién quieres saber sus amistades?     Escribe el número correspondiente "))
    elsujeto <- as.numeric(elsujeto) #El objeto con la respuesta se cambia a clase numérica y se reasigna al
                                     #mismo objeto
    
    
    #Para obtener qué personas son consideradas amigos por la persona que se eligió se usa la función
    #which, como elementos de la función se selecciona de la matriz el renglon de la matriz usando [] colocando el 
    #objeto con la respuesta previa, que indica el renglón específico de esa persona. Como condición se establece
    #cuáles son los que tienen 1 en ese renglón, que corresponde a las personas que esa persona considera sus amigos
    #lo guarda en un objeto.
    amig <- which ( redam[elsujeto,] == 1 )
    amig <- names(amig) #Con la función de names selecciona los nombres del objeto previo donde están 
                        #las personas que considera amigos la persona en cuestión. Lo reasigna al mismo
                        #objeto
    print(paste("Las personas consideradas como amigos por esta persona son : "))
    #Después arroja el mensaje al usuario indicando las personas que la persona seleccionada considera sus amigos
    print(paste(amig)) #Imprime el objeto de los nombres de los amigos de esa persona
    
    
    #Para saber qué personas consideran a la persona seleccionada su amig@ se realiza lo mismo que el 
    #paso anterior, solo que en lugar del renglón de la persona, es la columna, donde se encuentran los 1
    #que indican que otras personas consieran amigo@ a esa persona
    amig2 <- which(redam[,elsujeto]==1) #se guarda en otro objeto
    amig2 <- names(amig2) #se seleccionan los nombres y se reasigna al mismo objeto
    print(paste("Las personas que consideran amig@ a esta persona son : "))
    #arroja el mensaje al usuario indicando las personas que consideran amigo@ a la persona seleccionada
    print(paste(amig2)) #Imprime el objeto con los nombres de las personas que l@ consideran su amig@
    
    
    #Se realizó un ciclo for para que itire sobre los elementos del vector de los nombres de las personas 
    #que la persona seleccionada considera sus amigos, creado previamente. 
    for ( x in amig ){ #En el ciclo se estblece la condición de que para cada elmento x en el vector de 
                       #nombres de los amigos realice :
        print(paste("Hola ", x)) #Que imprima el mensaje "Hola" y el elemento de la posición en la que está
                                 #del vector de amigos, que corresponde a cada nombre de los amigos de la 
                                 #persona
    }
    
    
    #Para los dos últimos puntos se necesita de la red creada en igraph. No se hace plot de la gráfica
    #Se usa la opción graph_from_adjacency_matrix para crear la red a partir de la matriz previa de amistades
    #Se especifica que la red es dirigida. Se guarda en un objeto
    red_amigos <- graph_from_adjacency_matrix(ra, mode = "directed")
    
    
    #Para obtener el promedio de los amigos de todos se obtuvo primero el degree de la red creada previamente
    #Se especifica que sea el degree de salida, para tomar cuántas personas son consideradas amigos por cada 
    #uno, es decir todas las salidas de cada nodo. La función del degree está anidada dentro de la función mean
    #Por lo que inmediatamente se calcula el promedio de las salidas del degree, de los amigos de cada uno
    #Lo guarda en un objeto
    amigosxtodos <- mean ( degree( red_amigos, mode = "out") )
    print(paste("El promedio de amigos de todos es ", amigosxtodos))
    #Envia un mensaje al usuario indicando el promedio de los amigos de todos es el objeto del promedio del 
    #degree de salidas. Lo que serpia calcular el promedio de los amigos de todos.
    
    
    #Para calcular el coeficiente de clusterización se usa la función transitivity(). Como argumentos 
    #se tiene a la red de amistades creada, y se especifica que calcule el coeficiente a nivel local. 
    #Se usan los [] con la variable de selección de la persona para que muestre solo el coeficiente de 
    #clusterización de esa persona en especifico. Se guarda en un objeto.
    c_red <- transitivity(red_amigos, type = "local")[elsujeto]
    print(paste("El coeficiente de clusterizacion de los amigos de esta persona es ", c_red))
    #Se envía un mensaje al usuario indicando el coeficiente de clusterización es el objeto guardado
    #anteriormente, que contiene el coeficiente a nivel local y seleccionando a esa persona en específico
}
#Se corre la función
problema_3()



# 4. Utiliza la red del club de Karate de Zachary ( investiga como puedes 
#    generarla en igraph)
#       a) Calcula los nodos ms conectados
#       b) Calcula el dimetro
#       c) Encuentra la distribucin de conectividades.
#       d) Genera la matriz de adyacencia.
#       e) Dibuja la red con los nodos proporcionales al degree de cada uno de
#          ellos.
#install.packages("igraphdata")

#Para trabajar con la red del club de karate se instaló la libreria de igraphdata
library(igraphdata) #se carga la libreria
data(karate) #se carga un data set, en este caso, el de karate


#Para obtener las propiedades de la red se hizo una función sin argumentos de entrada y se guardó
#en el objeto karate_kid
karate_kid <- function(){
    
    #Arroja el mensaje al usuario indicanco que vea la red de karate
    print(paste("La red del karate se ve asi --> "))
    plot(karate,
         vertex.label.cex = 0.75, 
         vertex.label.family= "sans", vertex.label.color = "Black", 
         vertex.label.font = 2,
         edge.color = "dodgerblue1") #Se hace el plot de la red modificando variables estéticas
    
    
    #Para encontrar los nodos con más conexiones se creo un objeto que contiene el número de 
    #conexiones más alto que se encontró en la red. Para eso se ordenó el degree de la red 
    #en forma decresiente y se seleccionó el primer valor, que corresponde al número máximo
    #de conexiones.
    mc <- sort( degree(karate), decreasing = T )[1]
    mc <- mc - 1 #Después al valor maximo de conexiones se le resta 1, así, el 
    #objeto que contenia el valor máximo de conexiones queda con una unidad menos y se reasigna
    #al mismo objeto
    conec <- which( degree(karate) > mc) #Con la función which, del degree de la red, se 
    #seleccionan aquellos nodos que tienen un degree mayor al valor de la variable reasignada
    #Esto indica cuáles nodos específicamente tienen el número más alto de conexiones, reflejado
    #como el degree más alto, porque tienen un degree mayor al de la variable, y la variable tiene
    #el valor del degree más alto menos uno, lo que deja solamente los nodos cuyo degree es igual al 
    #valor de la variable inicialmente, lo que sería el degree más alto. Los nodos los guarda en un objeto.
    print(paste("Los nodos que tienes más conexiones son : ")) #Envía un mensaje al usuario para 
    #indicar cuáles nodos son los que tienen el degree más alto
    print(paste(conec)) #Se imprime el objeto de los nodos con el degree mayor 
    print(paste("Con ", mc + 1, " conexiones")) #Envia un mensaje al usuario para indicar que 
    #cuántas conexiones tienen los nodos con más conexiones, y esto lo hace sumando un 1 a la variable
    #a la que previamente se le había restado para regresar el valor original e indicar el número
    #mas alto de conexiones
    
    
    #Manda un mensaje al usuario para indicar el diámetro de la red
    print(paste( "El diámetro de la red es : ", diameter(karate) ))
    ##para establecer el diámetro se usa la función diameter del paquete 
    #igraph
    print(paste("La ruta que siguió para trazar el diámetro fue : ")) ##envia un mensaje al usuario para 
    #indicar la ruta que se siguió para calcular el diámetro
    print(paste(get.diameter(karate))) #Se imprime la ruta establecida con la función get.diameter de
    #la red
    
    
    #Para la distribución de conectividades arroja un mensaje al usuario indicando 
    #que se representa como una gráfica
    print("La distribución de conectividades se observa así --> ")
    #Par hacer la gráfica se hace un histograma del degree.distribution de la red con
    #la funcion hist y la función degree.distribution. Para la gráfica se modificaron
    #variables estéticas
    hist(degree.distribution(karate),
         main = "Distribución de conectividades", xlab = "degree distribution", 
         ylab = "Pk", border = "dodgerblue3", col = "midnightblue")
    
    
    #Se genera una matriz de adyacencia usando la función get.adjacency de la red y convirtiéndolo
    #a formato matriz, y lo guarda en un objeto.
    ma_k <- as.matrix(get.adjacency(karate))
    #Imprime un mensaje al usuario indicando la matriz de adyacencia
    print("La matriz de adyacencia es esta : ")
    View(ma_k) #Se visualiza la matriz de adyacencia 
    
    
    #Se arroja el mensaje al usuario indicando  el tamaño de los nodos proporcionales al degree 
    print("El tamaño de los nodos proporcionales al degree se observa así --> ")
    V(karate)$size <- degree(karate) #Para hacer el tamaño proporcional, se seleccionan los nodos de
    #la red con la función V, y se selecciona la propiedad de tamaño con el accesor $. Se asigna 
    #al tamaño de los nodos el degree de la red
    plot(karate) #Se hace el plot de la red con la modificación del tamaño
    
}
#Se pueba la función, no requiere argumentos de entrada
karate_kid()



#fresco el pana foráneo


