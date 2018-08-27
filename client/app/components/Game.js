import React from 'react'

import ActionPanel from './ActionPanel'
import Board from './Board'

const Game = props => {
  const { game, username, isTurnToAct } = props

  if (game) {
    const jsgame = game.toJS()
    jsgame._deck = undefined // hide deck
    console.log(jsgame)

    return (<div className='game'>
      <p style={{ height: '200px' }}>
        {(JSON.stringify({ ...jsgame, isTurnToAct, username }, undefined, '\n'))}
      </p>
      <div className='game-container'>
        <div className="table-container">
          <Board cards={game.get('_board')} />
        </div>
      </div>
      <ActionPanel {...props} />

    </div>)
  }
  return <h2>no Game State</h2>
}

export default Game