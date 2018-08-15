import React from 'react'

const Game = ({ username, isTurnToAct, game }) => {
    console.log('game', game)
    if (game) {
        const jsgame = game.toJS()
        jsgame._deck = undefined // hide deck
        return <p>{(JSON.stringify({ ...jsgame, isTurnToAct, username }, null, 20))}</p>
    }
    else {
        return <h2>no Game State</h2>
    }
}

export default Game