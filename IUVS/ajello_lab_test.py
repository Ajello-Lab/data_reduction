from scipy.io import readsav
import matplotlib.pyplot as plt


# Path to the IDL save file
file_path = '/Users/holsclaw/MAVEN/Ajello_lab/_Big_e-gun_RoundVII/data_reduction/CO2_30EV_FUV_TEST3_CO2_30EV_ROT_+7_IMAGE2_HI_PRESS_3E-5.idl'
# Load the data
data = readsav(file_path)

# Access the contents
print(data.keys())  # Print the keys in the dictionary

image_data = data['arr']  # Replace 'image_variable_name' with the actual key

# Display the image
plt.figure(figsize=(10, 6))
plt.imshow(image_data, cmap='inferno')  # Use 'gray' for grayscale images
plt.colorbar()  # Add a colorbar
plt.title('Image Display')
plt.show()



print('Done')

pass