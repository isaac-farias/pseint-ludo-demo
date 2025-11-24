

// simulador de ludo en pseint 
// autor: isaac farias 

//variables-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

Algoritmo LudoGame
	Definir tablero,dados, datos,fichas, zonas_de_llegada  Como Entero // arreglos
	Definir turno, cantj, eleccion, opc, cantdados, estados, cant_fichas, datos_ficha, contj, contf, cont_turnos,i,j,pasos,sum Como Entero // variables 
	Definir juego_terminado Como Logico
	// inicializacion de variables globales -----------------------------------------------------------------------------------------------------
	cant_fichas <- 4 
	datos_ficha<- 3 // id de la ficha y progreso 
	cantdados<-2
	eleccion<-0
	cantj<-2 
	estados<-3 // en casa, en juego, y en la meta 
	cont_turnos<-0
	juego_terminado<- Falso
	
	//- arreglos --------------------------------------------------------------------------------------------------------------------------------
	dimension dados[2]
	dimension fichas[datos_ficha,cantj*4]
	dimension datos[cantj,estados]
	dimension tablero[32]  // filas: las primeras dos para poder mover las fichas y la tercera un valor fijo que representara la casilla equivalente
	dimension zonas_de_llegada[4,4] //  4 posibles caminitos con 4 casillas cada uno inicializado de forma similar al tablero 
	// cada caminito debe tener spacio para albergar una ficha es decir 2x1
	//y ademas una fila extra para saber a que matriz dentro de la interfaz grafica corresponde esa casilla especifica 
	
	// inicializacion  de arreglos --------------------------------------------------------------------------------------------------------------
	
	// inicializacion del array que contendra la situacion de cada jugador 
	para i<-0  hasta cantj-1
		para j<-0 hasta estados-1
			si j== 0 Entonces
				datos[i,j]<- cant_fichas // inicializa una matriz con tres posibles estados de las fichas del jugador 
				// 0 siendo las fichas en casa al principio del juego 
				// 1 siendo las fichas en el tablero 
				// 2 siendo las fichas que ya llegaron a la meta 
			sino 
				datos[i,j]<- 0 
			FinSi
		FinPara
	FinPara
	
	// inicializacion de fichas 
	contj<-1
	contf<-0
	para j<-0 hasta (cantj*cant_fichas)-1
		si contf==cant_fichas 
			contj<- contj+1
			contf<-0
		FinSi
		contf<-contf+1
		fichas[0,j]<-(contj*10)+contf // primer digito a que jugador pertenece,segundo digito numero de ficha
		fichas[1,j]<-0// ninguna ficha tiene progreso al principio de la partida 
		segun contj
			caso 1: fichas[2,j]<- -1 // posiciones iniciales 
			caso 2: fichas[2,j]<- -1
		FinSegun
	FinPara
	
	// inicializacion de arreglo resultados de las tiradas  inicialmente cero  
//			para i<- 0 hasta cantdados-1 Hacer
//				dados[i]<-0 
//			FinPara
	dados[0]<-0
	dados[1]<-0
	
	// en  la funcion que muestra el tablero las matrices estan numeradas del 0 al 80 
	// el recorrido de una ficha de ludo sin embargo no es lineal y empieza en distintas partes del tablero
	
	// Inicializar tablero
		para j<-0 hasta 32-1
			tablero[j]<- 0 
		FinPara

	
	// inicializar zonas de llegada 
	para i<- 0 hasta 4-1 Hacer
		para j<-0 hasta 4-1
			zonas_de_llegada[i,j]<- 0 
		FinPara
	FinPara
	

	
	
	//bucle principal-------------------------------------------------------------------------------------------------------------------------------------------------------
	
	// la partida estara en un mientras gigante, la condcion sera que la matriz datos en [2, x] de al menos uno de los jugadores sea 4 
	// oalguno de los dos se retire 
	// un contador del la cantidad de turnos y un indicador de a que jugador le pertence dicho turno datos para poder pasarle a las funciones
	
	mostrar_titulo
	turno<-1 
	mientras juego_terminado <> Verdadero
		cont_turnos= cont_turnos+1 
		escribir"turnos jugados: ",cont_turnos
		si (cont_turnos<> 1) y (( cont_turnos mod 2)==0)
			turno<-2
		SiNo
			turno<-1 
		FinSi
		mostrar_tablero(tablero)
		escribir "turno del jugador: ",turno
		escribir ""
		escribir "-----presione tecla para lanzar dado-----"
		esperar tecla 
		tirada(dados)
		sum<- dados[0]+dados[1]
		pasos<- trunc(sum/cantdados)
		opc <- mostrar_opciones(datos,dados, turno)
		verificar_opcion(opc,datos, tablero, dados, fichas, zonas_de_llegada, turno, cant_fichas,pasos)
		Esperar Tecla
		Limpiar Pantalla
	FinMientras	
	
FinAlgoritmo




//-----funciones _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

subproceso mostrardatos( jugador1, jugador2)
	// esta funcion mostrara la informacion de las fichas de cada jugador 
FinSubProceso


subproceso tirada(dados)//_________________________________________________________________________________________________________________________________________________
	definir res como entero 
	res = aleatorio( 1, 6)
	dados[0] <- res 
	res = aleatorio( 1, 6)
	dados[1] <- res 
	escribir"salio [",dados[0],"] y [", dados[1],"]" 
finsubproceso  


funcion op <- mostrar_opciones(datos, dados, turno)//______________________________________________________________________________________________________________________
    definir arr, op Como Entero
    dimension arr[4] 
    arr[0]<-0 
    arr[1]<-0 
    arr[2]<-3 
    arr[3]<-4 
    escribir "" 
    escribir "=== opciones ==="
    definir max Como Entero
    max<-6
		si ((dados[0] == max) o (dados[1] == max)) y (datos[turno-1,0] >=1)
			escribir " 1. sacar ficha de casa "
			arr[0]<-1
		FinSi 
		si datos[turno-1,1]>=1
			escribir "2. mover ficha"
			arr[1]<-2
		FinSi
    escribir "3. saltar turno "
    escribir "4. retirarse "
    op<- pedir_dato(arr)
FinFuncion


funcion dato<- pedir_dato(arr)//___________________________________________________________________________________________________________________________________________
	definir dato Como Entero
	leer dato
	si arr[0]== 0
		si arr[1]==0 Entonces
			si arr[2]==0 
				mientras( dato<> arr[3] ) 
					escribir " ingrese una opcion valida"
					leer dato 
					Limpiar Pantalla
				FinMientras
			FinSi
			mientras(dato<> arr[2] y dato<> arr[3] ) 
				escribir " ingrese una opcion valida"
				leer dato
				Limpiar Pantalla
			FinMientras
		SiNo
			mientras(dato<> arr[1] y dato<> arr[2] y dato <> arr[3] ) 
				escribir " ingrese una opcion valida"
				leer dato 
				Limpiar Pantalla
			FinMientras
		finsi 
	sino
		mientras(dato<> arr[0] y dato<> arr[1] y dato <> arr[2] y dato <> arr[3] ) 
			escribir " ingrese una opcion valida"
			leer dato 
			Limpiar Pantalla
		FinMientras
	FinSi
	escribir ""
	escribir "opcion elegida: ",dato 
FinFuncion


subproceso verificar_opcion(opc,datos, tablero, dados, fichas, zonas_de_llegada, turno, cant_fichas,pasos)//_______________________________________________________________
	si opc==1 Entonces
		sacar_ficha(opc,datos, tablero, dados, fichas, zonas_de_llegada, turno, cant_fichas,pasos)
	sino 
		si opc==2 Entonces
			moverficha( datos,tablero, fichas, zonas_de_llegada, turno, pasos, cant_fichas )
		sino 
			si opc ==3
				escribir""
				Escribir " -- has saltado tu turno --" 
			SiNo
				si opc ==4 
					mostrar_titulo 
				FinSi
			FinSi
		FinSi
	finsi 
FinSubProceso


subproceso mostrar_titulo//________________________________________________________________________________________________________________________________________________
	
	definir i, j, fila Como Entero
	
	Limpiar Pantalla
	
	Escribir "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
	Para fila <- 1 Hasta 3 Hacer
		Escribir "||                                                              ||"
	FinPara 
	Escribir "||              ===========================                     || "
	Escribir "||            ___              _______     _____                 | _"
	Escribir "||          = |###   ___   ___ |######    #####| =-               |."
	Escribir "||         --= |##=- |###= |###- |# |##--##| =##| =-                /       .  _ "
	Escribir "||   <---- --= |##=-- |##= |##-= |#  |## #| ===#| =--   ---->              [presione enter] "
	Escribir "||         --= |##=-- |##= |##-= |#  |## #| ==_#| =--                      "
	Escribir "||         --=_|##___ |##_ |##-- |#_|##--##|__##| =-                | /_  -."
	Escribir "||          = |###### |##__|## |######    #####| =-                 ||"
	Escribir "||                     |#####                                    |  _"
	Escribir "||               ======-----===============                      ||"
	Para i <- 1 Hasta 2 Hacer
		Escribir "||                                                              ||"
	FinPara
	Escribir "||      °º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸           ||"
	Para i <- 1 Hasta 2 Hacer
		Escribir "||                                                              ||"
	FinPara
	Para i <- 1 Hasta 2 Hacer
		Escribir "||                                                              ||"
	FinPara
	Escribir "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
	Escribir ""
    Esperar Tecla
	Limpiar Pantalla

	// comienzo de la partida 
	Escribir "=== NUEVA PARTIDA DE LUDO ==="
	Escribir "Jugadores: 2"
	Escribir "Objetivo: Llevar todas tus fichas a la meta"
	Escribir ""
	Escribir "REGLAS DEL JUEGO:"
	Escribir "- Lanza dos dados en cada turno"
	Escribir "- Necesitas un 6 para sacar fichas de casa"
	Escribir "- Puedes capturar fichas de otros jugadores (excepto en zonas seguras)"
	Escribir "- Las zonas seguras son: las casillas de la zona de llegada de cada jugador"
	Escribir "- Gana el primer jugador en llevar sus 4 fichas a la meta"
	Escribir ""
	Escribir "Presiona Enter para comenzar..."
	esperar tecla 
	Limpiar Pantalla
FinSubProceso


subproceso moverficha( datos,tablero, fichas, zonas_de_llegada, turno, pasos,cant_fichas )//_______________________________________________________________________________
	// esta funcion necesitara la cantidad de casillas a mover que por conveniencia y tamaño del tablero seran los resultados obtenidos sobre la cantidad de dados 
	// necesitara que el id de la ficha a mover previamente se verifica si esta ficha esta en juego mediante la func puede_mover
	//  se validara el moviemiento mediante validaraccion, si dicha func devuelve 1 la ficha en esa casilla se enviara a la casa correspondiente y se quitara del tablero
	//ademas de actualizar la informacion del jugador afectado
	//  por ultimo realiza el cambia de poicion de la ficha en el tablero y suma esa misma cantidad de casillas en el progreso  de la ficha mediante actualizar info 
	definir  ficha_elegida, stop,j,i, arr, id ,accion,casilla_actual,situacion como entero 
	id<-0 
	dimension arr[4]
		para i<-0 hasta 4-1 Hacer
			arr[i]<-0 
		finpara 
		escribir "que ficha desea mover" 
		
	verificar_fichas_disponibles(turno,cant_fichas,fichas, arr )  // muestra y guarda las fichas que estan en el recorrido principal de ese jugador 
	ficha_elegida<- pedir_dato(arr)    // pide un dato y verifica que sea una ficha que esta dentro del tablero
	id <- (turno*10)+ficha_elegida    // id de la ficha elegida 
	accion <-validaraccion(turno, pasos,tablero,id, fichas)// verifica  en cual de los tres posibles casos que pueden darse al avanzar una ficha se encuentra el jugador 
	casilla_actual<- averiguar_pos(tablero,id)
		segun accion Hacer
			caso 0: 
				situacion<-0 
				actualizar_info(pasos,turno, datos,situacion, casilla_actual,ficha_elegida,fichas)
				cambiar_pos(tablero, casilla_actual,pasos)
				escribir "ficha ",ficha_elegida," en posicion ", casilla_actual +pasos
				
			caso 1:  //  mueve la ficha existente en esa casilla a la casa de su jugador y resetea su progreso para luego ejecutar el caso 0 
				situacion<-1 
				actualizar_info(pasos,turno, datos,situacion, casilla_actual,ficha_elegida,fichas)
				si turno ==1 
					fichas[0, (tablero[casilla_actual+pasos]) mod 10 ]<-tablero[casilla_actual+pasos]
				sino 
					fichas[0, ((tablero[casilla_actual+pasos]) mod 10)+4 ]<-tablero[casilla_actual+pasos]
				finsi 
				tablero[casilla_actual+pasos]<-0
				cambiar_pos(tablero, casilla_actual,pasos)
				escribir "ficha ",ficha_elegida," en posicion ", casilla_actual+pasos
				
			caso 2:   // imprime un mensaje y selcciona otra accion
				escribir "ya hay una ficha tuya  en esa casilla debes moverla primero" 
				opc <- mostrar_opciones(datos,dados, turno)
				verificar_opcion(opc,datos, tablero, dados, fichas, zonas_de_llegada, turno, cant_fichas,pasos)
				
			caso 3: //ficha en el tramo final 
				situacion<-3 
				actualizar_info(pasos,turno, datos,situacion, casilla_actual,ficha_elegida,fichas)
				escribir "ficha ",ficha_elegida," en posicion ", casilla_actual +pasos
				
			caso 4: // ficha en la meta 
				situacion<-4 
				actualizar_info(pasos,turno, datos,situacion, casilla_actual,ficha_elegida,fichas)
				escribir "ficha ",ficha_elegida," en posicion ", casilla_actual+pasos 
				
				
		FinSegun
FinSubProceso


subproceso cambiar_pos(tablero, casilla_actual,pasos)//____________________________________________________________________________________________________________________
	si     (casilla_actual+pasos) >31
		tablero[(casilla_actual+pasos)-30]<-tablero[casilla_actual]	
		tablero[casilla_actual]<- 0
		casilla_actual<- (casilla_actual+pasos)-30
	sino
		tablero[casilla_actual+pasos]<-	tablero[casilla_actual]		// intercambio de posicion de la ficha elegida
		tablero[casilla_actual]<- 0
		casilla_actual<- casilla_actual+pasos
	FinSi
FinSubProceso


SubProceso sacar_ficha(opc,datos, tablero, dados, fichas, zonas_de_llegada, turno, cant_fichas,pasos)//____________________________________________________________________
	definir i ,indice como entero
	i<-0
	si turno == 1 
		si tablero[0]=0 
			si fichas[0,0]<>0 Entonces
				indice<-0 
				salir_de_casa(tablero, fichas, indice, turno, cant_fichas)
			sino 
				si fichas[0,1]<>0 
					indice<-1 
					salir_de_casa(tablero, fichas, indice, turno, cant_fichas)
				SiNo
					si fichas[0,2]<>0 
						indice<-2 
						salir_de_casa(tablero, fichas, indice, turno, cant_fichas)
					sino
						si fichas[0,3]<>0 
							indice<-3 
							salir_de_casa(tablero, fichas, indice, turno, cant_fichas)
						FinSi
					FinSi
				FinSi
			FinSi
			escribir "ficha ",indice+1," en posicion 0..."
		SiNo
			escribir "ya hay una ficha tuya en esa casilla debes moverla primero" 
			opc <- mostrar_opciones(datos,dados, turno)
			verificar_opcion(opc,datos, tablero, dados, fichas, zonas_de_llegada, turno, cant_fichas,pasos)
		finsi 
	finsi 
	
	si turno ==2
		si tablero[16]=0
			si fichas[0,4]<>0 Entonces
				indice<-4 
				salir_de_casa(tablero, fichas, indice, turno, cant_fichas)
			sino 
				si fichas[0,5]<>0 
					indice<-5 
					salir_de_casa(tablero, fichas, indice, turno, cant_fichas)
				SiNo
					si fichas[0,6]<>0 
						indice<-6 
						salir_de_casa(tablero, fichas, indice, turno, cant_fichas)
					sino
						si fichas[0,7]<>0 
							indice<-7 
							salir_de_casa(tablero, fichas, indice, turno,cant_fichas)
						FinSi
					FinSi
				FinSi
			FinSi
			escribir "ficha ",indice-3," en posicion 16"
		sino 
			escribir "ya hay una ficha tuya en esa casilla debes moverla primero" 
			opc <- mostrar_opciones(datos,dados, turno)
			verificar_opcion(opc,datos, tablero, dados, fichas, zonas_de_llegada, turno, cant_fichas,pasos)
		FinSi
	FinSi
	datos[turno-1,0]<-datos[turno-1,0]-1
	datos[turno-1,1]<-datos[turno-1,1]+1
FinSubProceso


subproceso salir_de_casa(tablero, fichas, indice, turno, cant_fichas)//____________________________________________________________________________________________________
	si turno ==1 Entonces
		tablero[0]<-fichas[0,indice]
		fichas[1,indice]<-0
		fichas[2,indice]<-0
	SiNo
		si turno ==2 Entonces
			tablero[16]<-fichas[0,indice]
			fichas[1,indice]<-0
			fichas[2,indice]<-16
		FinSi
	FinSi
FinSubProceso


SubProceso verificar_fichas_disponibles(turno,cantfichas,fichas, arr )//___________________________________________________________________________________________________
	definir i,j, stop,cant_fichas como entero 
	j<-0
	i<-0
	cant_fichas<-4
		segun turno
			caso 1:
				para i<-0 hasta 4-1  //fichas disponibles para mover juador 1
					si fichas[2,i]<> -1 
						arr[j]<-i+1
						j<-j+1
						escribir "ficha",i+1,"(",i+1,")"
					FinSi
					finpara 
			caso 2:
				para i<-4 hasta 8-1  //fichas disponibles para mover juador 2
					si fichas[2,i]<> -1 
						arr[j]<-i-3
						j<-j+1
						escribir "ficha",i-3,"(",i-3,")"
					FinSi
				finpara 
		FinSegun

FinSubProceso


subproceso actualizar_info(pasos,turno, datos,situacion, casilla_actual,ficha_elegida,fichas)//______________________________________________________________________________
	// cada turno se actualizara la posicion de cada ficha en el tablero principal con el indice de columna donde se halla, esa sera su casilla,
	//ya partir de ahi se planterian las verificaciones 
	// este dato se econtrara alojado permanentemente en fichas en la posicion en la que se inicializo la ficha para poder hallarla facilmente sin tener que hacer una busqudda 
	// actualiza las fichas del jugador de donde se validara que fichas tiene el jugador en juego 
	// fichas en juego sera un una matriz de filas== cantidad columnas == cant fichas de jugadores  que contendra las fichas del jugador que estan en el juego, 
	// las que estan en casa estaran en otra matriz de igual dimension 
	segun situacion Hacer
		caso 0:  
		caso 1: 
		caso 3:
		caso 4:
	FinSegun
FinSubProceso


funcion val <-validaraccion(turno, pasos,tablero,id, fichas)//______________________________________________________________________________________________________________
	// esta funcion ve en donde caeria la ficha elegida y verifica 1 que no exista otra ficha en esa casilla para poder mover 
	// devovlera 0 si se puede mover 
	// devolvera 1 si hay una ficha de otro jugador en el lugar y dicha ficha se enviara a la casa
	// devolvera 2 si hay una ficha de otro jugador en el lugar y dicha ficha se enviara a la casa
	// devolvera 3 si la ficha ya ha completado el circuito principal y pasara a uno de los caminos 
	// devovlera 4 si la ficha ya ha llegado a la meta 
	// ficha_elegida es un valor entero de 1 al 4 por lo que para asociciarla a un jugador necesitaremos tambien el turno 
	// con estos dos datos ya podemos buscar la ficha por id, recordemos que el id se encuentra en la fila cero de cada ficha y del tablero 
	definir casilla_actual, casilla_futura, val como entero 
	casilla_actual <- averiguar_pos(tablero, id) //posicion actual de la ficha a mover
	casilla_futura <- casilla_actual +pasos 
		si casilla_futura >= 32
			casilla_futura<- casilla_futura-32 
		FinSi
		si fichas[1, (turno*(id mod 10))-1] >=34 // evalua el progreso de la ficha para saber que debe hacer el programa 
			val<-3  
		SiNo
			si fichas[1, (turno*(id mod 10))-1] >=38  
				val<-4
			SiNo
				si tablero[casilla_futura] == 0 Entonces
					val<-0 
				sino 
					si trunc(tablero[casilla_futura]/10) <>  trunc(tablero[casilla_actual]/10) 
						val <- 1 
					sino 
						si trunc(tablero[casilla_futura]/10) ==  trunc(tablero[casilla_actual]/10)
							val <- 2
						FinSi
					FinSi
				FinSi
			FinSi
		FinSi
FinFuncion


funcion casilla_actual <-averiguar_pos(tablero,id)//________________________________________________________________________________________________________________________
	definir casilla_actual,i como entero 
	para i<-0 hasta 32-1 Hacer
		si tablero[i] == id Entonces
			casilla_actual <- i
		FinSi
	FinPara
FinFuncion


SubProceso mostrar_tablero(tablero)//____________________________________________________________________________________________________________________________________
	// LÍNEA 1 A 50: el tablero completo
    Escribir "                             |¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨|"
    Escribir "                             |                  SIMULADOR DE LUDO               |"
    Escribir "                             |__________________________________________________|"
    Escribir ""
	// CAMINO EXTERNO 38 casillas (dibujado con caracteres)
	Escribir "         o----------------------------------------------------------------------------------------o"
	Escribir "         |                                                                                        |"  
	Escribir "         |                                                                                        |"
    Escribir "         |                             ___________________________________                        |"
    Escribir "         |                            |           | ",tablero[19]," | ",tablero[18]," | ",tablero[17]," |           |                       |"
    Escribir "         |                          | |           |---|---|---|           | |                     |"
    Escribir "         |                         <| |           | ",tablero[20]," |   | ",tablero[16]," |           | |>                    |"
    Escribir "         |                        <>| |           |---|   |---|           | |<>                   |"
    Escribir "         |                       <><| |           | ",tablero[21]," |   | ",tablero[15]," |           | |><>                  |"
    Escribir "         |                     <> <>| |---------------|   |---------------| |<> <>                |" 
    Escribir "         |                   <>  <><| | ",tablero[25]," | ",tablero[24]," | ",tablero[23]," | ",tablero[22]," |   | ",tablero[14]," | ",tablero[13]," | ",tablero[12]," | ",tablero[11]," | |><>  <>              |"
    Escribir "         |                 <>  <> <>| |---------------o   o---------------| |<> <>  <>            |"
    Escribir "         |              <>  <>  <> <| | ",tablero[26]," |             X             | ",tablero[10]," | |> <>  <>  <>         |"
    Escribir "         |           <>   <>  <>  <>| |---------------o   o---------------| |<>  <>  <>   <>      |"
    Escribir "         |              <>  <>  <> <| | ",tablero[27]," | ",tablero[28]," | ",tablero[29]," | ",tablero[30]," |   | ",tablero[6]," | ",tablero[7]," | ",tablero[8]," | ",tablero[9]," | |> <>  <>  <>         |"
    Escribir "         |                 <>  <> <>| |---------------|   |---------------| |<> <>  <>            |"
    Escribir "         |                   <>  <><| |           | ",tablero[31]," |   | ",tablero[5]," |           | |><>  <>              |"
    Escribir "         |                     <> <>| |           |---|   |---|           | |<> <>                |"
    Escribir "         |                       <><| |           | ",tablero[0]," |   | ",tablero[4]," |           | |><>                  |"
    Escribir "         |                        <>| |           |---|---|---|           | |<>                   |"
    Escribir "         |                         <| |           | ",tablero[1]," | ",tablero[2]," | ",tablero[3]," |           | |>                    |"
	Escribir "         |                        <>| |____________-----------____________| |<>                   |"
	Escribir "         |                         <|_______________________________________|>                    |"
	Escribir "         |                        <><><><><><><><><><><><><><><><><><><><><><>                    |"
	Escribir "         |                                                                                        |"
	Escribir "         |                                                                                        |"
	Escribir "         |                                                                                        |"
	Escribir "         o----------------------------------------------------------------------------------------o"
    Escribir "                                           "
    Escribir ""  
    Escribir "                       ??????? META ???????"
    Escribir ""  
FinSubProceso



	