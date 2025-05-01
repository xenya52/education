# Sorting Algorithms

This [site](https://www.programiz.com/dsa/sorting-algorithm) helped a lot.

## Complexity of Sorting Algorithms

Understanding the complexity of sorting algorithms is essential when evaluating their efficiency.

### Time Complexity

Time complexity describes how the runtime of an algorithm increases with the input size.

#### Big-O notation (O)
- Describes the **worst-case** scenario.
- Tells us how the algorithm performs as the input grows.
- Example: Bubble Sort has O(n²) in the worst case.

#### Omega notation (Ω)
- Describes the **best-case** scenario.
- Represents the minimum time the algorithm will take.
- Example: Insertion Sort has Ω(n) if the list is already sorted.

#### Theta notation (Θ)
- Describes the **average-case** scenario.
- Gives a tight bound for an algorithm's running time.
- Example: Merge Sort has Θ(n log n) in all cases.

---

## Actual Sorting Algorithms

### 1. Bubble Sort
- Repeatedly compares adjacent elements and swaps them if they are in the wrong order.
- **Time Complexity**: O(n²), Ω(n)
- **Space Complexity**: O(1)
- **Stable**: ✅

### 2. Insertion Sort
- Builds the sorted list one item at a time by inserting elements into the correct position.
- **Time Complexity**: O(n²), Ω(n)
- **Space Complexity**: O(1)
- **Stable**: ✅

### 3. Selection Sort
- Selects the smallest element from the unsorted part and swaps it with the first unsorted element.
- **Time Complexity**: O(n²)
- **Space Complexity**: O(1)
- **Stable**: ❌ (unless modified)

### 4. Merge Sort
- Divides the array into halves, sorts them, and merges the sorted halves.
- **Time Complexity**: O(n log n) in all cases
- **Space Complexity**: O(n)
- **Stable**: ✅

### 5. Quick Sort
- Picks a pivot, partitions the array into two halves, and recursively sorts them.
- **Time Complexity**: O(n log n) average, O(n²) worst
- **Space Complexity**: O(log n) (recursive stack)
- **Stable**: ❌

### 6. Heap Sort
- Builds a heap from the array and extracts the maximum repeatedly.
- **Time Complexity**: O(n log n)
- **Space Complexity**: O(1)
- **Stable**: ❌

---

## Additional Resources

- [Visualgo - Visualize sorting algorithms](https://visualgo.net/en/sorting)
- [GeeksforGeeks - Sorting Algorithms](https://www.geeksforgeeks.org/sorting-algorithms/)

