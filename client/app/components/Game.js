import React from 'react'

import ActionPanel from './ActionPanel'

const Game = props => {
  const { game, username, isTurnToAct } = props

  if (game) {
    const jsgame = game.toJS()
    jsgame._deck = undefined // hide deck
    console.log(game)

    return (<div className='game'>
      <div className="gameTable" />
      <ActionPanel {...props} />
    </div>)
  }
  return <h2>no Game State</h2>
}

export default Game