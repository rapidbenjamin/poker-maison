import React from 'react'

import ActionPanel from './ActionPanel'
import Board from './Board'
import Seat from './Seat'


const getSeatedPlayer = (player, gameStage, position, isTurnToAct) =>
  <Seat
    key={position}
    position={position}
    playerName={player.get('_playerName')}
    chips={player.get('_chips')}
    hasPocketCards={player.get('_playerState') === 'In' && gameStage !== 'PreDeal'}
    isTurnToAct={isTurnToAct && gameStage !== 'preDeal' && gameStage !== 'Showdown'}
  />


const getSeats = (maxPlayers, players, gameStage, currentPosToAct) =>
  Array(maxPlayers).fill(null).map((_, i) => {
    const player = players.get(i)
    const isTurnToAct = i === currentPosToAct

    return player ? getSeatedPlayer(player, gameStage, i, isTurnToAct) :
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
    const userPlayer = game.get('_players').find(p => p.get('_playerName') === username)
    const userPocketCards = userPlayer ? userPlayer.get('_pockets') : null
    const currentPosToAct = game.get('_currentPosToAct')

    return (<div className='game-view-grid'>
      <p style={{ height: '300px', top: '80px', position: 'absolute' }}>
        {(JSON.stringify({ ...jsgame, isTurnToAct, username }, undefined, '\n'))}
      </p>
      <div className='game-container'>
        <div className='table-container'>
          {getSeats(maxPlayers, players, gameStage, currentPosToAct)}
          <div className='game-grid'>
            <Board cards={game.get('_board')} />
            <h4 className='pot-label'>{`Pot $${game.get('_pot')}`}</h4>
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