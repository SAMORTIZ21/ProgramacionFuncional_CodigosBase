module App.Menus

open System
open App.Utils

type MainMenuChoice =
    | CargarPartida
    | PartidaNueva
    | Salir

type PauseMenuChoice =
    | Resume
    | Menu

type GameOverChoice =
    | Retry
    | MainMenu

let private opcionTextoMain = function
    | CargarPartida -> "                   Cargar partida"
    | PartidaNueva -> "                   Partida Nueva"
    | Salir -> "                   Salir"

let private opcionTextoGameOver = function
    | Retry -> "                    Volver a intentarlo"
    | MainMenu -> "                   Menu Principal"

let private dibujarMenuPrincipal seleccion =
    Console.Clear()
    Console.CursorVisible <- false
    Console.ForegroundColor <- ConsoleColor.White
    Console.WriteLine("                   ╔════════════════════════╗")
    Console.WriteLine("                   ║      MENU PRINCIPAL    ║")
    Console.WriteLine("                   ╚════════════════════════╝")
    Console.WriteLine()
    Console.WriteLine("               Usa ↑ ↓ y Enter para seleccionar")
    Console.WriteLine()
    [ CargarPartida; PartidaNueva; Salir ]
    |> List.iteri (fun i opcion ->
        if i = seleccion then
            Console.ForegroundColor <- ConsoleColor.Green
            Console.WriteLine($"▶ {opcionTextoMain opcion}")
        else
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.WriteLine($"  {opcionTextoMain opcion}"))
    Console.ResetColor()

let private opcionTextoPausa = function
    | Resume -> "                    Volver a la partida"
    | Menu -> "                          Menu"

let private dibujarMenuPausa seleccion =
    Console.Clear()
    Console.CursorVisible <- false
    Console.ForegroundColor <- ConsoleColor.White
    Console.WriteLine("                   ╔════════════════════════╗")
    Console.WriteLine("                   ║         PAUSA          ║")
    Console.WriteLine("                   ╚════════════════════════╝")
    Console.WriteLine()
    Console.WriteLine("               Usa ↑ ↓ y Enter para seleccionar")
    Console.WriteLine()
    [ Resume; Menu ]
    |> List.iteri (fun i opcion ->
        if i = seleccion then
            Console.ForegroundColor <- ConsoleColor.Green
            Console.WriteLine($"▶ {opcionTextoPausa opcion}")
        else
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.WriteLine($"  {opcionTextoPausa opcion}"))
    Console.ResetColor()

let mostrarMenuPrincipal () =
    let opciones = [ CargarPartida; PartidaNueva; Salir ]
    let rec loop seleccion =
        dibujarMenuPrincipal seleccion
        match Console.ReadKey true with
        | key when key.Key = ConsoleKey.UpArrow ->
            loop ((seleccion + opciones.Length - 1) % opciones.Length)
        | key when key.Key = ConsoleKey.DownArrow ->
            loop ((seleccion + 1) % opciones.Length)
        | key when key.Key = ConsoleKey.Enter ->
            opciones.[seleccion]
        | _ ->
            loop seleccion
    loop 0

let private dibujarMenuGameOver seleccion =
    Console.Clear()
    Console.CursorVisible <- false
    Console.ForegroundColor <- ConsoleColor.White
    Console.WriteLine("                   ╔════════════════════════╗")
    Console.WriteLine("                   ║      GAME OVER         ║")
    Console.WriteLine("                   ╚════════════════════════╝")
    Console.WriteLine()
    Console.WriteLine("               Usa ↑ ↓ y Enter para seleccionar")
    Console.WriteLine()
    [ Retry; MainMenu ]
    |> List.iteri (fun i opcion ->
        if i = seleccion then
            Console.ForegroundColor <- ConsoleColor.Green
            Console.WriteLine($"▶ {opcionTextoGameOver opcion}")
        else
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.WriteLine($"  {opcionTextoGameOver opcion}"))
    Console.ResetColor()

let mostrarMenuGameOver () =
    let opciones = [ Retry; MainMenu ]
    let rec loop seleccion =
        dibujarMenuGameOver seleccion
        match Console.ReadKey true with
        | key when key.Key = ConsoleKey.UpArrow ->
            loop ((seleccion + opciones.Length - 1) % opciones.Length)
        | key when key.Key = ConsoleKey.DownArrow ->
            loop ((seleccion + 1) % opciones.Length)
        | key when key.Key = ConsoleKey.Enter ->
            opciones.[seleccion]
        | _ ->
            loop seleccion
    loop 0

let mostrarMenuPausa () =
    let opciones = [ Resume; Menu ]
    let rec loop seleccion =
        dibujarMenuPausa seleccion
        match Console.ReadKey true with
        | key when key.Key = ConsoleKey.UpArrow ->
            loop ((seleccion + opciones.Length - 1) % opciones.Length)
        | key when key.Key = ConsoleKey.DownArrow ->
            loop ((seleccion + 1) % opciones.Length)
        | key when key.Key = ConsoleKey.Enter ->
            opciones.[seleccion]
        | _ ->
            loop seleccion
    loop 0
