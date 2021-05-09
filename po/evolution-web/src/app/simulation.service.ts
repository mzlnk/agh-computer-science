import {Injectable} from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class SimulationService {

  simulations: Map<string, Simulation> = new Map();

  constructor() {
    this.loadSimulations();
    this.schedule();
  }

  private loadSimulations() {
    for (let i = 0; i < 5; i++) {
      const size = getRandomInteger(20, 60);
      const ets = [];
      for (let j = 0; j < 50; j++) {
        ets.push(new Entity({
          id: 'a1',
          type: (Math.random() <= 0.7 ? 'Animal' : 'Grass'),
          x: getRandomInteger(0, size),
          y: getRandomInteger(0, size),
          age: getRandomInteger(1, 230),
          generation: 'XII',
          genes: [1, 2, 3, 4, 5, 6, 7, 8],
          energy: 45
        }));
      }

      const sm = new Simulation({
        id: 'simulation-' + String(i),
        properties: new SimulationProperties({
          id: 'simulation-' + String(i),
          name: 'Simulation no.' + String(i),
          mapSize: size,
          jungleSize: 15,
          timeCreated: '2019/12/08 20:00',
          lifetime: '10 years 0 months 0 days',
          currentAge: '2 years 11 months 16 days',
          maxAnimalEnergy: 200,
          grassInitEnergy: 20
        }),
        entities: ets
      });

      this.simulations.set(sm.id, sm);
    }
  }

  private schedule() {
    setInterval(() => {
      this.updateEntities();
    }, 50);
  }

  private updateEntities() {
    for (let sm of this.simulations.values()) {
      const mapSize = sm.properties.mapSize;
      for (let entity of sm.entities) {
        entity.energy = Math.min(Math.max(entity.energy + getRandomInteger(-2, 3), 0), 100);
        entity.age++;
        entity.x = getRandomInteger(0, mapSize);
        entity.y = getRandomInteger(0, mapSize);
      }
    }

  }

}

export class Entity {

  id: string;
  type: string;
  x: number;
  y: number;
  age: number;
  generation: string;
  genes: number[];
  energy: number;

  constructor(private data: any) {
    this.id = data.id;
    this.type = data.type;
    this.x = data.x;
    this.y = data.y;
    this.age = data.age;
    this.generation = data.generation;
    this.genes = data.genes;
    this.energy = data.energy;
  }

}

export class SimulationProperties {

  id: string;
  name: string;
  mapSize: number;
  jungleSize: number;
  timeCreated: string;
  lifetime: string;
  currentAge: string;
  maxAnimalEnergy: number;
  grassInitEnergy: number;

  constructor(private data: any) {
    this.id = data.id;
    this.name = data.name;
    this.mapSize = data.mapSize;
    this.jungleSize = data.jungleSize;
    this.timeCreated = data.timeCreated;
    this.lifetime = data.lifetime;
    this.currentAge = data.currentAge;
    this.maxAnimalEnergy = data.maxAnimalEnergy;
    this.grassInitEnergy = data.grassInitEnergy;
  }

}

export class Simulation {

  id: string;
  entities: Entity[];
  properties: SimulationProperties;

  constructor(private data: any) {
    this.id = data.id;
    this.entities = data.entities;
    this.properties = data.properties;
  }

}

function getRandomInteger(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min)) + min;
}
