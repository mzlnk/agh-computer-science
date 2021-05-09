package pl.mzlnk.po.lab4;

import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;

public class MoveDirectionTest {

    @Test
    public void findByShortcut() {
        assertArrayEquals(OptionParser.parse(new String[]{"g"}), new MoveDirection[]{});
    }

}