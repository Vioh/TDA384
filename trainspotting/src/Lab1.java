import TSim.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.Semaphore;

public class Lab1 {
    public Lab1(int speed1, int speed2) {
        Map.initialize();

        Map.Track track1 = (Map.Track) Map.getSection(new Map.Position(15, 7));
        Map.Track track2 = (Map.Track) Map.getSection(new Map.Position(5, 11));

        Train train1 = new Train(1, speed1, Map.Direction.TowardsA, track1);
        Train train2 = new Train(2, speed2, Map.Direction.TowardsB, track2);

        train1.start();
        train2.start();
    }
}

class Train extends Thread {
    private int id;
    private int speed;
    private Map.Track prevTrack;
    private Map.Direction travelDirection;
    private TSimInterface tsi = TSimInterface.getInstance();

    public Train(int id, int speed, Map.Direction initialTravelDirection, Map.Track initialTrack) {
        this.id = id;
        this.speed = speed;
        this.prevTrack = null;
        this.travelDirection = initialTravelDirection;
        initialTrack.acquire();
    }

    @Override
    public void run() {
        try {
            tsi.setSpeed(id, speed);
            while (true) processSensorEvent(tsi.getSensor(this.id));
        }
        catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    private void processSensorEvent(SensorEvent event) throws Exception {
        Map.Position sensor = new Map.Position(event.getXpos(), event.getYpos());
        Map.Section currSection = Map.getSection(sensor);

        if (currSection instanceof Map.Track) {
            Map.Track currTrack = (Map.Track) currSection;

            if (event.getStatus() == SensorEvent.INACTIVE && hasReachedStartOfSection(currTrack)) {
                if (prevTrack != null) prevTrack.release();
                prevTrack = null;
            }
            if (event.getStatus() == SensorEvent.ACTIVE && hasReachedEndOfSection(currTrack)) {
                tsi.setSpeed(id, 0);
                if (currTrack.acquireNextTrack()) {
                    prevTrack = currTrack;
                } else {
                    waitAndTurnBack();
                }
                tsi.setSpeed(id, speed);
            }
        }
        else {
            if (event.getStatus() == SensorEvent.INACTIVE && hasReachedEndOfSection(currSection)) {
                currSection.release();
            }
            if (event.getStatus() == SensorEvent.ACTIVE && hasReachedStartOfSection(currSection)) {
                tsi.setSpeed(id, 0);
                currSection.acquire();
                tsi.setSpeed(id, speed);
            }
        }
    }

    /**
     * Check if the train has reached the end of a given directed section.
     *
     * Since a sensor is always placed at the end of a directed section, the train reaches the end
     * if the section has the same travelling direction as the train itself.
     */
    private boolean hasReachedEndOfSection(Map.Section section) {
        return this.travelDirection == section.getTravelDirection();
    }

    /**
     * Check if the train has reached the start of a given directed section.
     *
     * Since the start of a section is where we place the sensor for the directed section heading
     * in the opposite direction, so the train enters the start of the given section only when the
     * section has the opposite travelling direction as the train itself.
     */
    private boolean hasReachedStartOfSection(Map.Section section) {
        return this.travelDirection != section.getTravelDirection();
    }

    /**
     * Wait at the station for some time and then turn back.
     */
    private void waitAndTurnBack() throws Exception {
        int waitTime = 1000 + (20 * Math.abs(speed));
        Thread.sleep(waitTime);

        if (this.travelDirection == Map.Direction.TowardsA) {
            this.travelDirection = Map.Direction.TowardsB;
        } else {
            this.travelDirection = Map.Direction.TowardsA;
        }
        this.speed = -this.speed;
    }
}

///////////////////////////////////////////////////////////////////////////////
// MAP UTILITIES FOR TSIM
///////////////////////////////////////////////////////////////////////////////

class Map {
    private static HashMap<Position, Section> sensorsToSectionsMapping = null;

    public static void initialize() {
        sensorsToSectionsMapping = new HashMap<>();
        Semaphore[] semaphores = new Semaphore[9];
        Position[] switches = new Position[4];
        Section[] sections = new Section[4];
        Track[] tracks = new Track[16];

        for (int i = 0; i < semaphores.length; i++) {
            semaphores[i] = new Semaphore(1);
        }

        switches[0] = new Position(3, 11);
        switches[1] = new Position(4, 9);
        switches[2] = new Position(15, 9);
        switches[3] = new Position(17, 7);

        sections[0] = new Section(Direction.TowardsB, semaphores[0], new Position(6, 7));
        sections[1] = new Section(Direction.TowardsB, semaphores[0], new Position(8, 5));
        sections[2] = new Section(Direction.TowardsA, semaphores[0], new Position(10, 7));
        sections[3] = new Section(Direction.TowardsA, semaphores[0], new Position(9, 8));

        tracks[0] = new Track(Direction.TowardsB, semaphores[1], new Position(5, 11));
        tracks[1] = new Track(Direction.TowardsB, semaphores[2], new Position(3, 13));
        tracks[2] = new Track(Direction.TowardsB, semaphores[3], new Position(2, 9));
        tracks[3] = new Track(Direction.TowardsB, semaphores[4], new Position(13, 9));
        tracks[4] = new Track(Direction.TowardsB, semaphores[5], new Position(13, 10));
        tracks[5] = new Track(Direction.TowardsB, semaphores[6], new Position(19, 7));
        tracks[6] = new Track(Direction.TowardsB, semaphores[7], new Position(16, 3));
        tracks[7] = new Track(Direction.TowardsB, semaphores[8], new Position(16, 5));

        tracks[8] = new Track(Direction.TowardsA, semaphores[1], new Position(16, 11));
        tracks[9] = new Track(Direction.TowardsA, semaphores[2], new Position(16, 13));
        tracks[10] = new Track(Direction.TowardsA, semaphores[3], new Position(1, 11));
        tracks[11] = new Track(Direction.TowardsA, semaphores[4], new Position(6, 9));
        tracks[12] = new Track(Direction.TowardsA, semaphores[5], new Position(6, 10));
        tracks[13] = new Track(Direction.TowardsA, semaphores[6], new Position(17, 9));
        tracks[14] = new Track(Direction.TowardsA, semaphores[7], new Position(15, 7));
        tracks[15] = new Track(Direction.TowardsA, semaphores[8], new Position(15, 8));

        tracks[0].addConnection(tracks[2], switches[0], TSimInterface.SWITCH_LEFT);
        tracks[1].addConnection(tracks[2], switches[0], TSimInterface.SWITCH_RIGHT);
        tracks[2].addConnection(tracks[3], switches[1], TSimInterface.SWITCH_LEFT);
        tracks[2].addConnection(tracks[4], switches[1], TSimInterface.SWITCH_RIGHT);
        tracks[3].addConnection(tracks[5], switches[2], TSimInterface.SWITCH_RIGHT);
        tracks[4].addConnection(tracks[5], switches[2], TSimInterface.SWITCH_LEFT);
        tracks[5].addConnection(tracks[6], switches[3], TSimInterface.SWITCH_RIGHT);
        tracks[5].addConnection(tracks[7], switches[3], TSimInterface.SWITCH_LEFT);

        tracks[10].addConnection(tracks[8], switches[0], TSimInterface.SWITCH_LEFT);
        tracks[10].addConnection(tracks[9], switches[0], TSimInterface.SWITCH_RIGHT);
        tracks[11].addConnection(tracks[10], switches[1], TSimInterface.SWITCH_LEFT);
        tracks[12].addConnection(tracks[10], switches[1], TSimInterface.SWITCH_RIGHT);
        tracks[13].addConnection(tracks[11], switches[2], TSimInterface.SWITCH_RIGHT);
        tracks[13].addConnection(tracks[12], switches[2], TSimInterface.SWITCH_LEFT);
        tracks[14].addConnection(tracks[13], switches[3], TSimInterface.SWITCH_RIGHT);
        tracks[15].addConnection(tracks[13], switches[3], TSimInterface.SWITCH_LEFT);
    }

    public static Section getSection(Position sensor) {
        if (sensorsToSectionsMapping == null) {
            throw new RuntimeException("Map for TSim is uninitialized!");
        }
        return sensorsToSectionsMapping.get(sensor);
    }

    /**
     * Map direction, where A is the bottom station, and B is the top station.
     */
    public static enum Direction {
        TowardsA,
        TowardsB;
    }

    /**
     * Map position, identified by the x-y coordinates.
     */
    public static class Position {
        public final int x;
        public final int y;

        public Position(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public int hashCode() {
            return x * 100 + y;
        }

        @Override
        public boolean equals(Object pos) {
            return this.hashCode() == pos.hashCode();
        }
    }

    /**
     * Section is a directed rail section on the map that allows a train to travel in a certain
     * direction, either towards station A or station B.
     *
     * It has a single sensor placed at the end, and shares the same resources (i.e. semaphore)
     * with the section that supports travelling in the opposite direction.
     */
    public static class Section {
        private Semaphore semaphore;
        private Direction travelDirection;

        public Section(Direction travelDirection, Semaphore semaphore, Position sensor) {
            this.semaphore = semaphore;
            this.travelDirection = travelDirection;
            sensorsToSectionsMapping.put(sensor, this);
        }

        public boolean isAvailable() {
            return semaphore.availablePermits() > 0;
        }

        public Direction getTravelDirection() {
            return this.travelDirection;
        }

        /**
         * Blocking call to wait for the section to be available for acquiring.
         */
        public void acquire() {
            try {
                semaphore.acquire();
            } catch (InterruptedException ignored) {
            }
        }

        /**
         * Release the resources so that the other train can occupy this section.
         */
        public void release() {
            semaphore.release();
        }
    }

    /**
     * Track is also a directed rail section. But unlike a normal section, it can be connected to
     * other tracks via the so-called connections (which actually contain the switches on the map).
     */
    public static class Track extends Section {
        private ArrayList<Connection> connections;

        public Track(Direction travelDirection, Semaphore semaphore, Position sensor) {
            super(travelDirection, semaphore, sensor);
            this.connections = new ArrayList<>();
        }

        public void addConnection(Track nextTrack, Position switchPos, int switchDir) {
            connections.add(new Connection(nextTrack, switchPos, switchDir));
        }

        /**
         * Blocking call to wait for the next track to be available for acquiring.
         *
         * @return true if success, and false if the train has reached one of the stations.
         */
        public boolean acquireNextTrack() throws CommandException {
            for (int i = 0; i < connections.size(); i++) {
                Connection connection = connections.get(i);
                Track nextTrack = connection.getNextTrack();

                if (i == connections.size() - 1 || nextTrack.isAvailable()) {
                    nextTrack.acquire();
                    connection.makeRailSwitch();
                    return true;
                }
            }
            return false;
        }
    }

    /**
     * Connection between a directed track and the next directed track on the map.
     */
    public static class Connection {
        private Track nextTrack;
        private Position switchPos;
        private int switchDir;

        public Connection(Track nextTrack, Position switchPos, int switchDir) {
            this.nextTrack = nextTrack;
            this.switchPos = switchPos;
            this.switchDir = switchDir;
        }

        public Track getNextTrack() {
            return this.nextTrack;
        }

        public void makeRailSwitch() throws CommandException {
            TSimInterface.getInstance().setSwitch(switchPos.x, switchPos.y, switchDir);
        }
    }
}
