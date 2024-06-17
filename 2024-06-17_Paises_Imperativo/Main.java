import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

class ColumnComparator implements Comparator<List<String>> {
    private int column;

    public ColumnComparator(int column) {
        this.column = column;
    }

    @Override
    public int compare(List<String> row1, List<String> row2) {
        int value1 = Integer.parseInt(row1.get(column));
        int value2 = Integer.parseInt(row2.get(column));
        return Integer.compare(value1, value2);
    }
}

public class Main {

    // indexes of the columns in the csv file
    private static final int COUNTRY = 0;
    private static final int CONFIRMED = 1;
    private static final int DEATHS = 2;
    private static final int RECOVERY = 3;
    private static final int ACTIVE = 4;

    // splits a string by commas
    private static List<String> parseCSVLine(String line) {
        return Arrays.asList(line.split(","));
    }

    // returns the sum of the values of the nth column of a list of lists
    private static int sumValue(List<List<String>> data, int column) {
        int sum = 0;

        for (List<String> row : data) {
            int value = Integer.parseInt(row.get(column));
            sum += value;
        }
        
        return sum;
    }

    // returns the sum of the values of the first x lists of a list of lists
    private static int sumOfXValues(List<List<String>> data, int x, int column) {
        int sum = 0;

        for (int i = 0; i < x; i++) {
            int value = Integer.parseInt(data.get(i).get(column));
            sum += value;
        }
        
        return sum;
    }

    // returns the list of lists with the values of the given column or equal to n
    private static List<List<String>> listGreaterOrEqualThan(String[][] data, int n, int column) {
        List<List<String>> list = new ArrayList<>();
        
        for(int i = 0; i < data.length; i++) {
            int value = Integer.parseInt(data[i][column]);
            if(value >= n) {
                list.add(Arrays.asList(data[i]));
            }
        }

        return list;
    }

    // returns n lists with the greatest value of the given column
    private static List<List<String>> listGreatestValue(String[][] data, int n, int column) {
        List<List<String>> list = new ArrayList<>();

        for(int i = 0; i < data.length; i++) {
            list.add(Arrays.asList(data[i]));
        }

        list.sort(new ColumnComparator(column));

        Collections.reverse(list);

        list = list.subList(0, n);
        
        return list;
    }

    // returns n lists with the smallest value of the given column
    private static List<List<String>> listSmallestValue(List<List<String>> data, int n, int column) {
        List<List<String>> list = new ArrayList<>(data);

        list.sort(new ColumnComparator(column));

        return list.subList(0, n);
    }

    // returns the sum of active of all the countries with confirmed greater or equal to n
    private static int firstCase(String[][] data, int n) {
        return sumValue(listGreaterOrEqualThan(data, n, CONFIRMED), ACTIVE);
    }

    // between the x countries with the greatest active, returns the sum of deaths of the y countries with the lowest confirmed
    private static int secondCase(String[][] data, int x, int y) {
        List<List<String>> greatestActive = listGreatestValue(data, x, ACTIVE);
        List<List<String>> smallestConfirmed = listSmallestValue(greatestActive, y, CONFIRMED);
        return sumOfXValues(smallestConfirmed, y, DEATHS);
    }

    // returns the x countries with the greatest confirmed in alphabetical order
    private static List<String> thirdCase(String[][] data, int x) {
        List<List<String>> greatestConfirmed = listGreatestValue(data, x, CONFIRMED);
        List<String> list = new ArrayList<>();

        for (List<String> row : greatestConfirmed) {
            list.add(row.get(COUNTRY));
        }

        Collections.sort(list);

        return list;
    }

    public static void main(String[] args) {
        try {
            // Reading CSV file
            List<String> lines = Files.readAllLines(Paths.get("dados.csv"));

            // Parsing CSV file
            String[][] data = new String[lines.size()][5];

            for(int i = 0; i < lines.size(); i++) {
                data[i] = parseCSVLine(lines.get(i)).toArray(new String[0]);
            }

            // Reading inputs
            Scanner scanner = new Scanner(System.in);
            String command = scanner.nextLine();
            String[] inputs = command.split(" ");
            int n1 = Integer.parseInt(inputs[0]);
            int n2 = Integer.parseInt(inputs[1]);
            int n3 = Integer.parseInt(inputs[2]);
            int n4 = Integer.parseInt(inputs[3]);
            scanner.close();

            // Running cases
            System.out.println(firstCase(data, n1));
            System.out.println(secondCase(data, n2, n3));
            thirdCase(data, n4).forEach(System.out::println);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
