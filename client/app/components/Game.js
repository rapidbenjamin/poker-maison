import React from 'react'

import ActionPanel from './ActionPanel'
import Board from './Board'
import Seat from './Seat'
import Card from './Card'

const getSeatedPlayer = (username, player, gameStage, position, isTurnToAct) =>
  <Seat
    key={position}
    position={position}
    playerName={player.get('_playerName')}
    chips={player.get('_chips')}
    hasPocketCards={player.get('_playerName') !== username && player.get('_playerState') === 'In' && gameStage !== 'PreDeal' && gameStage !== 'Showdown'}
    isTurnToAct={isTurnToAct && gameStage !== 'Showdown'}
  />

const getSeats = (username, maxPlayers, players, gameStage, currentPosToAct) =>
  Array(maxPlayers).fill(null).map((_, i) => {
    const player = players.get(i)
    const isTurnToAct = i === currentPosToAct

    return player ? getSeatedPlayer(username, player, gameStage, i, isTurnToAct) :
      <Seat
        key={i}
        position={i}
      />
  })

const getPocketCards = players => players.map((p, i) =>
  p.get('_pockets').length !== 0 ?
    (<div className={`showdown-pocket-cards-${i}`} key={p.get('_playerName')} >
      <div className='showdown-pocket-cards-container' >
        {p.get('_pockets').map(card => {
          const rank = card.get('rank')
          const suit = card.get('suit')

          return (<Card
            key={rank + suit}
            rank={rank}
            suit={suit}
          />)
        })}
      </div>
    </div>) : ''
)

const Game = props => {
  const { game, username, isTurnToAct } = props

  if (game) {
    const jsgame = game.toJS()
    console.log(jsgame)
    const players = game.get('_players')
    const dealerPos = game.get('_dealer')
    const maxPlayers = 6
    const gameStage = game.get('_street')
    const potSize = game.get('_pot')
    const userPlayer = game.get('_players').find(p => p.get('_playerName') === username)
    const userPocketCards = userPlayer ? userPlayer.get('_pockets') : null
    const currentPosToAct = game.get('_currentPosToAct')
    const isMultiplayerShowdown = game.get('_winners').get('tag') == 'MultiPlayerShowdown'
    const showdownPots = game.get('_winners').get('contents')
    const mainShowdownPot = showdownPots ? showdownPots.get ? showdownPots.get(0) : null : null
    const mainShowdownPotHandRanking = mainShowdownPot ? mainShowdownPot.get(0).get(0) : null
    const mainShowdownPotHandCards = mainShowdownPot ? mainShowdownPot.get(0).get(1) : null
    const mainShowdownPotHandPlayers = mainShowdownPot ? mainShowdownPot.get(1) : null
    const playerNamesWinnersOfMainShowdownPot = mainShowdownPot ? mainShowdownPot.get(1) : null

    return (<div className='game-view-grid'>
      <p style={{ height: '300px', top: '80px', position: 'absolute' }}>
        {(JSON.stringify({ ...jsgame, isTurnToAct, username }, undefined, '\n'))}
      </p>
      <div className='game-container'>
        <div className='table-container'>
          {getPocketCards(players)}
          {getSeats(username, maxPlayers, players, gameStage, currentPosToAct)}
          <div className='game-grid'>
            {players ? players.count() > 1 ?
              <div className={`dealer-btn-pos-${dealerPos}`}>
              </div>
              : '' : ''}
            <Board cards={game.get('_board')} />
            <h4 className='pot-label'>
              <span className='monospaced-font'>
                {`$${potSize}`}
              </span>
            </h4>
            {mainShowdownPot ? <p className='winners-label'>
              {`${mainShowdownPotHandPlayers} wins with ${mainShowdownPotHandRanking}`}
            </p> : ''}
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