open System
open System.Threading
open App.Utils
open App.Menus

let maxAlienHits = 3
let hitResetDelayTicks = 50
let enemyMovementTickInterval = 4
let enemyFireTickInterval = 10
let gameLoopDelayMs = 25
let tickIncrement = 1
let alienMoveStep = 1
let enemyMoveStep = 1
let alienMissileStep = 1
let enemyMissileStep = 1
let initialTick = -1
let initialAlienX = Console.BufferWidth / 2
let initialAlienY = Console.BufferHeight / 2
let initialEnemyX = Console.BufferWidth - 2
let initialEnemyY = 0
let initialEnemyDir = 1
let alienShootOffset = 2
let enemyShootOffset = 2

let rightBound = Console.BufferWidth - 2
let bottomBound = Console.BufferHeight - 1
let leftBound = 0
let topBound = 0

type ProgramState =
| Starting
| Running
| Terminated

type SpriteState =
| Alive
| Hit
| Dead

type Misil = {
    X: int
    Y: int
}

type State = {
    ProgramState: ProgramState
    AlienX: int
    AlienY: int
    AlienState: SpriteState
    AlienHits: int
    RedibujarPantalla: bool
    Tick: int
    Misiles: Misil list
    EnemigoX: int
    EnemigoY: int
    EnemigoDir: int
    EnemigoEstado: SpriteState
    MisilesEnemigos: Misil list
    ColisionAlien: int
    ColisionEnemigo: int
}

let estadoInicial = {
    ProgramState = Starting
    AlienX = initialAlienX
    AlienY = initialAlienY
    AlienState = Alive
    AlienHits = 0
    RedibujarPantalla = true
    Tick = initialTick
    Misiles = []
    EnemigoX = initialEnemyX
    EnemigoY = initialEnemyY
    EnemigoDir = initialEnemyDir
    EnemigoEstado = Alive
    MisilesEnemigos = []
    ColisionAlien = 0
    ColisionEnemigo = 0
}

let dibujarSprite getX getY getSprite state =
    mostrarMensaje (getX state) (getY state) ConsoleColor.Yellow (getSprite state)

let dibujarAlien =
    dibujarSprite
        (fun s -> s.AlienX)
        (fun s -> s.AlienY)
        (fun s -> 
            match s.AlienState  with 
            | Alive -> "👽"
            | Hit -> "💥"
            | Dead -> "☠️")

let dibujarEnemigo =
    dibujarSprite
        (fun s -> s.EnemigoX)
        (fun s -> s.EnemigoY)
        (fun s -> 
            match s.EnemigoEstado with 
            | Alive -> "👾"
            | _ -> "💥")

let dibujarMisilesCon getMisiles color sprite state =
    state
    |> getMisiles
    |> List.iter (fun misil -> mostrarMensaje misil.X misil.Y color sprite)

let dibujarMisiles = dibujarMisilesCon (fun s -> s.Misiles) ConsoleColor.Yellow "=>"
let dibujarMisilesEnemigos = dibujarMisilesCon (fun s -> s.MisilesEnemigos) ConsoleColor.Red "<="

let redibujarPantalla state =
    if state.RedibujarPantalla then 
        Console.Clear()
        [|
            dibujarAlien
            dibujarMisiles
            dibujarEnemigo
            dibujarMisilesEnemigos
        |]
        |> Array.iter (fun f -> f state)
        {state with RedibujarPantalla=false}
    else
        state

let actualizarTick state =
    {state with Tick = state.Tick + tickIncrement}

let actualizarLista getItems setItems transform keep state =
    let items = getItems state
    if items = [] then state
    else
        let nuevos =
            items
            |> List.map transform
            |> List.filter keep
        setItems ({ state with RedibujarPantalla=true }) nuevos

let actualizarMisiles =
    actualizarLista
        (fun s -> s.Misiles)
        (fun s nuevos -> { s with Misiles = nuevos })
        (fun misil -> { misil with X = misil.X + alienMissileStep })
        (fun misil -> misil.X < rightBound)

let actualizarMisilesEnemigos =
    actualizarLista
        (fun s -> s.MisilesEnemigos)
        (fun s nuevos -> { s with MisilesEnemigos = nuevos })
        (fun misil -> { misil with X = misil.X - enemyMissileStep })
        (fun misil -> misil.X >= leftBound)

let actualizarDisparoEnemigo state =
    if state.EnemigoEstado = Alive && state.Tick % enemyFireTickInterval = 0 then 
        let nuevoMisil = {
            X = state.EnemigoX - enemyShootOffset
            Y = state.EnemigoY
        }
        {state with MisilesEnemigos = nuevoMisil :: state.MisilesEnemigos; RedibujarPantalla = true}
    else
        state
let actualizarEnemigo state =
    if state.EnemigoEstado = Alive && state.Tick % enemyMovementTickInterval = 0 then 
        let nuevaY = state.EnemigoY + state.EnemigoDir * enemyMoveStep
        match nuevaY with 
        | y when y > bottomBound -> bottomBound, -enemyMoveStep
        | y when y < topBound -> topBound, enemyMoveStep
        | y -> y, state.EnemigoDir
        |> fun (y,dir) ->
            {state with EnemigoY=y;EnemigoDir=dir;RedibujarPantalla=true}
    else
        state

let detectarColision getMisiles hitCondition onHit state =
    let misiles = getMisiles state
    let nuevosMisiles = misiles |> List.filter (fun misil -> not (hitCondition state misil))
    if nuevosMisiles.Length <> misiles.Length then
        onHit state nuevosMisiles
        |> fun s -> { s with RedibujarPantalla=true }
    else
        state

let detectarColisionConAlien =
    detectarColision
        (fun s -> s.MisilesEnemigos)
        (fun state misil -> misil.X = state.AlienX + 1 && misil.Y = state.AlienY)
        (fun state nuevosMisiles -> { state with AlienState = Hit; MisilesEnemigos = nuevosMisiles; ColisionAlien = state.Tick; AlienHits = state.AlienHits + 1 })

let detectarColisionConEnemigo =
    detectarColision
        (fun s -> s.Misiles)
        (fun state misil -> misil.X = state.EnemigoX - 1 && misil.Y = state.EnemigoY)
        (fun state nuevosMisiles -> { state with EnemigoEstado = Hit; Misiles = nuevosMisiles; ColisionEnemigo = state.Tick })

let resetHitSprite getSpriteState setSpriteState getColision state =
    if getSpriteState state = Hit then 
        let tiempo = state.Tick - getColision state
        if tiempo >= hitResetDelayTicks then 
            state
            |> setSpriteState
            |> fun s -> { s with RedibujarPantalla=true }
        else
            state
    else
        state

let resetAlien =
    resetHitSprite
        (fun s -> s.AlienState)
        (fun s -> { s with AlienState = Alive })
        (fun s -> s.ColisionAlien)

let resetEnemigo =
    resetHitSprite
        (fun s -> s.EnemigoEstado)
        (fun s -> { s with EnemigoEstado = Alive })
        (fun s -> s.ColisionEnemigo)

let detectarMuerte state =
    if state.AlienHits >= maxAlienHits then
        { state with AlienState = Dead; ProgramState = Terminated; RedibujarPantalla = true }
    else
        state
let procesarTecladoApp key state =
    match key with 
    | ConsoleKey.Escape ->
        match mostrarMenuPausa () with
        | Resume -> {state with RedibujarPantalla = true}
        | Menu -> { estadoInicial with ProgramState = Starting }
    | _ -> state
let procesarTecladoAlien key state =
    if state.AlienState = Alive then 
        match key with 
        | ConsoleKey.Spacebar ->
            let nuevoMisil = {
                X = state.AlienX + alienShootOffset
                Y = state.AlienY
            }
            {state with Misiles = nuevoMisil :: state.Misiles}
        | ConsoleKey.UpArrow ->
            {state with AlienY = max topBound (state.AlienY - alienMoveStep)}
        | ConsoleKey.DownArrow ->
            {state with AlienY = min bottomBound (state.AlienY + alienMoveStep)}
        | ConsoleKey.LeftArrow ->
            {state with AlienX = max leftBound (state.AlienX - alienMoveStep)}
        | ConsoleKey.RightArrow ->
            {state with AlienX = min rightBound (state.AlienX + alienMoveStep)}
        | _ -> state
        |> fun nuevoEstado ->
            if nuevoEstado <> state then 
                {nuevoEstado with RedibujarPantalla=true}
            else
                state
    else
        state

let procesarTeclado state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state
        |> procesarTecladoApp k.Key
        |> procesarTecladoAlien k.Key
    else
        state

let rec mainLoop state =
    state
    |> actualizarTick
    |> actualizarMisiles
    |> actualizarEnemigo
    |> actualizarDisparoEnemigo
    |> actualizarMisilesEnemigos
    |> detectarColisionConAlien
    |> detectarMuerte
    |> detectarColisionConEnemigo
    |> resetAlien
    |> resetEnemigo
    |> procesarTeclado
    |> redibujarPantalla
    |> fun nuevoEstado ->
        match nuevoEstado.ProgramState with
        | Terminated
        | Starting ->
            nuevoEstado
        | _ ->
            Thread.Sleep gameLoopDelayMs
            nuevoEstado |> mainLoop

let iniciarDesdeMenu state =
    match state.ProgramState with
    | Starting ->
        match mostrarMenuPrincipal () with
        | CargarPartida -> { estadoInicial with ProgramState = Running }
        | PartidaNueva -> { estadoInicial with ProgramState = Running }
        | Salir -> { state with ProgramState = Terminated }
    | _ -> state

let rec iniciarJuego state =
    let estadoInicialMenu = iniciarDesdeMenu state
    if estadoInicialMenu.ProgramState = Terminated then
        ()
    else
        let finalState = mainLoop estadoInicialMenu
        if finalState.AlienState = Dead then
            match mostrarMenuGameOver () with
            | Retry -> iniciarJuego estadoInicial
            | MainMenu -> iniciarJuego { estadoInicial with ProgramState = Starting }
        else
            match finalState.ProgramState with
            | Starting -> iniciarJuego { estadoInicial with ProgramState = Starting }
            | _ -> ()

Console.Clear()
Console.CursorVisible <- false

iniciarJuego estadoInicial

Console.Clear()
Console.CursorVisible <- true

