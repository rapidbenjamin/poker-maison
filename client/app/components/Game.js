import React from 'react'

import ActionPanel from './ActionPanel'
import Board from './Board'
import Seat from './Seat'


const getSeatedPlayer = (player, gameStage, position) =>
  <Seat
    key={position}
    position={position}
    playerName={player.get('_playerName')}
    chips={player.get('_chips')}
    hasPocketCards={player.get('_playerState') === 'In' && gameStage !== 'PreDeal'}
  />


const getSeats = (maxPlayers, players, gameStage) =>
  Array(maxPlayers).fill(null).map((_, i) => {
    const player = players.get(i)

    return player ? getSeatedPlayer(player, gameStage, i) :
      <Seat
        key={i}
        position={i}
      />
  })

const Game = props => {
  const { game, username, isTurnToAct } = props

  if (game) {
    const jsgame = game.toJS()
    jsgame._deck = undefined // hide deck
    console.log(jsgame)
    const players = game.get('_players')
    const maxPlayers = 6

    const gameStage = game.get('_street')

    console.log(game.get('_players').toJS())
    console.log(username)

    const userPlayer = game.get('_players').find(p => p.get('_playerName') === username)
    console.log(userPlayer)
    const userPocketCards = userPlayer ? userPlayer.get('_pockets') : null

    console.log(userPocketCards)
    return (<div className='game-view-grid'>
      <p style={{ height: '300px', top: '80px', position: 'absolute' }}>
        {(JSON.stringify({ ...jsgame, isTurnToAct, username }, undefined, '\n'))}
      </p>
      <div className='game-container'>
        <div className="table-container">
          {getSeats(maxPlayers, players, gameStage)}
          <div className='game-grid'>
            <Board cards={game.get('_board')} />
            <h2 className='pot-label'>{`Pot $${game.get('_pot')}`}</h2>
          </div>
          <div className='game-table'>
          </div>
        </div>
      </div>
      <ActionPanel {...props} gameStage={gameStage} userPocketCards={userPocketCards} />

    </div>)
  }
  return <h2>no Game State</h2>
}

export default Game