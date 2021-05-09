package pl.mzlnk.po.lab3;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
import static pl.mzlnk.po.lab3.MoveDirection.*;

public class OptionsParserTest {

    private String[] stringList1, stringList2;
    private MoveDirection[] directionList1, directionList2;

    @Before
    public void setUp() {
        stringList1 = new String[] {"f", "forward", "backward", "b"};
        stringList2 = new String[] {"l", "right", "left", "r"};

        directionList1 = new MoveDirection[] {FORWARD, FORWARD, BACKWARD, BACKWARD};
        directionList2 = new MoveDirection[] {LEFT, RIGHT, LEFT, RIGHT};
    }

    @Test
    public void parse() {
        assertArrayEquals(directionList1, OptionsParser.parse(stringList1));
        assertArrayEquals(directionList2, OptionsParser.parse(stringList2));
    }

}