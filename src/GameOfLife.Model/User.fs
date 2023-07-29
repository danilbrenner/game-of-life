namespace GameOfLife.Model

module User =

    type Settings = { theme: Theme; name: string }

    and Theme =
        | Light
        | Dark

    let defaultSettings = { name = "Guest"; theme = Dark }

    let toggleTheme =
        function
        | { theme = Dark } as settings -> { settings with theme = Light }
        | { theme = Light } as settings -> { settings with theme = Dark }
