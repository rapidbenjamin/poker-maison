import React from 'react';

const Seat = ({ playerName, chips, isTurnToAct, hasPocketCards, position }) => (
  <div className={`seat-${position}-container ${isTurnToAct ? 'active-player' : ''}`}>
    {hasPocketCards ?
      <div className='hidden-pocket-cards' >
        <div className='hidden-pocket-cards-container' >
          <div className='card pocket-one' />
          <div className='card pocket-two' />
        </div>
      </div> : ''}
    <div className={`seat-${position} ${playerName ? '' : 'empty-seat'}`}>
      <h4 className={playerName ? 'player-name' : ''}>{playerName || 'Seat'}</h4>
      {playerName ? <h4 className='player-chip-count'>{chips}</h4> : ''}
    </div>
  </div>);

export default Seat;