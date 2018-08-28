import React from 'react'
import ActionPanel from './ActionPanel'
import Board from './Board'
import Seat from './Seat'


const getSeatedPlayer = (player, position) =>
  <Seat
    key={position}
    position={position}
    playerName={player.get('_playerName')}
    chips={player.get('_chips')}
  />

const getSeats = (maxPlayers, players) =>
  Array(maxPlayers).fill(null).map((_, i) => {
    const player = players.get(i)

    return player ? getSeatedPlayer(player, i) :
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

    return (<div className='game-view-grid'>
      <p style={{ height: '100px' }}>
        {(JSON.stringify({ ...jsgame, isTurnToAct, username }, undefined, '\n'))}
      </p>
      <div className='game-container'>
        <div className="table-container">
          {getSeats(maxPlayers, players)}
          <div className='game-grid'>
            <Board cards={game.get('_board')} />
            <h2 className='pot-label'>{`Pot $${game.get('_pot')}`}</h2>
          </div>
          <div className='game-table'>
          </div>
        </div>
      </div>
      <ActionPanel {...props} />

    </div>)
  }
  return <h2>no Game State</h2>
}

export default Game