data LevelSelectState  = LevelSelectState {example :: String}
data StartScreenState  = StartScreenState {example2 :: String}
data LevelPlayingState  = LevelPlayingState {example3 :: String}

data GameState = LevelSelectState_ LevelSelectState | StartScreenState_ StartScreenState | LevelPlayingState_ LevelPlayingState
