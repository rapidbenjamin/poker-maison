import React from 'react';

const Seat = ({ playerName, chips, isTurnToAct, position }) => (
  <div className={`seat-${position}`}>
    <h3>{playerName || 'Seat'}</h3>
    {chips || ''}
  </div>);

export default Seat;