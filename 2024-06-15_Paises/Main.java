import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

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
        return data.stream()
                .mapToInt(row -> Integer.parseInt(row.get(column)))
                .sum();
    }

    // returns the sum of the values of the first x lists of a list of lists
    private static int sumOfXValues(List<List<String>> data, int x, int column) {
        return sumValue(data.stream().limit(x).collect(Collectors.toList()), column);
    }

    // returns the list of lists with the values of the given column or equal to n
    private static List<List<String>> listGreaterOrEqualThan(List<List<String>> data, int n, int column) {
        return data.stream()
                .filter(row -> Integer.parseInt(row.get(column)) >= n)
                .collect(Collectors.toList());
    }

    // returns n lists with the greatest value of the given column
    private static List<List<String>> listGreatestValue(List<List<String>> data, int n, int column) {
        return data.stream()
                .sorted(Comparator.comparingInt((List<String> row) -> Integer.parseInt(row.get(column))).reversed())
                .limit(n)
                .collect(Collectors.toList());
    }

    // returns n lists with the smallest value of the given column
    private static List<List<String>> listSmallestValue(List<List<String>> data, int n, int column) {
        return data.stream()
                .sorted(Comparator.comparingInt(row -> Integer.parseInt(row.get(column))))
                .limit(n)
                .collect(Collectors.toList());
    }

    // returns the sum of active of all the countries with confirmed greater or equal to n
    private static int firstCase(List<List<String>> data, int n) {
        return sumValue(listGreaterOrEqualThan(data, n, CONFIRMED), ACTIVE);
    }

    // between the x countries with the greatest active, returns the sum of deaths of the y countries with the lowest confirmed
    private static int secondCase(List<List<String>> data, int x, int y) {
        List<List<String>> greatestActive = listGreatestValue(data, x, ACTIVE);
        List<List<String>> smallestConfirmed = listSmallestValue(greatestActive, y, CONFIRMED);
        return sumOfXValues(smallestConfirmed, y, DEATHS);
    }

    // returns the x countries with the greatest confirmed in alphabetical order
    private static List<String> thirdCase(List<List<String>> data, int x) {
        return listGreatestValue(data, x, CONFIRMED).stream()
                .map(row -> row.get(COUNTRY))
                .sorted()
                .collect(Collectors.toList());
    }

    public static void main(String[] args) {
        try {
            // Reading CSV file
            List<String> lines = Files.readAllLines(Paths.get("dados.csv"));

            // Parsing CSV file
            List<List<String>> data = lines.stream()
                    .map(Main::parseCSVLine)
                    .collect(Collectors.toList());

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
